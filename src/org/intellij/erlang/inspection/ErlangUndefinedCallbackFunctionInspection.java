/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import kotlin.reflect.jvm.internal.impl.utils.SmartList;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangCreateFunctionQuickFix;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

public class ErlangUndefinedCallbackFunctionInspection extends ErlangInspectionBase {
  public static final String FIX_MESSAGE = "Implement and export all callbacks";

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    boolean supportOptionalCallbacks = release == null || !ErlangSdkRelease.V_18_0.isNewerThan(release);

    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ErlangModuleRef behaviourRef = behaviour.getModuleRef();
      ErlangFile behaviourModule = ErlangPsiImplUtil.resolveToFile(behaviourRef);
      if (behaviourModule == null) continue;

      List<ErlangCallbackSpec> undefinedCallbacks = new SmartList<>();
      Map<String, ErlangCallbackSpec> callbackMap = behaviourModule.getCallbackMap();
      for (ErlangCallbackSpec spec : callbackMap.values()) {
        if (supportOptionalCallbacks && spec.isOptional()) continue;

        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        ErlangFunction function = name != null ? file.getFunction(name, arity) : null;
        if (function == null || !function.isExported()) {
          undefinedCallbacks.add(spec);
        }
      }
      if (undefinedCallbacks.isEmpty()) continue;

      boolean multiple = undefinedCallbacks.size() != 1;
      StringBuilder builder = new StringBuilder();
      builder.append("Undefined or non-exported callback function").append(multiple ? "s" : "").append(": ");
      for (ErlangCallbackSpec spec : undefinedCallbacks) {
        builder.append("'").append(ErlangPsiImplUtil.createFunctionPresentationFromCallbackSpec(spec)).append("', ");
      }
      String message = builder.substring(0, builder.length() - 2);
      registerProblem(problemsHolder, behaviourRef, message,
                      new CreateAndExportFunctionsFix(undefinedCallbacks, file.getProject()));
    }
  }

  private static class CreateAndExportFunctionsFix extends LocalQuickFixBase {
    private final List<SmartPsiElementPointer<ErlangCallbackSpec>> myCallbackSpecs;

    public CreateAndExportFunctionsFix(@NotNull List<ErlangCallbackSpec> callbackSpecs, @NotNull Project project) {
      super(FIX_MESSAGE);
      final SmartPointerManager manager = SmartPointerManager.getInstance(project);
      myCallbackSpecs = ContainerUtil.map(callbackSpecs, manager::createSmartPsiElementPointer);
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      PsiElement problemElement = problemDescriptor.getPsiElement();
      PsiFile containingFile = problemElement != null ? problemElement.getContainingFile() : null;
      ErlangFile file = ObjectUtils.tryCast(containingFile, ErlangFile.class);
      if (file == null) return;

      addCallbackImplementations(project, file);
      exportAddedCallbackImplementations(project, file);
    }

    private void addCallbackImplementations(@NotNull Project project, @NotNull ErlangFile file) {
      PsiDocumentManager manager = PsiDocumentManager.getInstance(project);
      Document document = manager.getDocument(file);
      if (document == null) return;

      for (SmartPsiElementPointer<ErlangCallbackSpec> pointer : myCallbackSpecs) {
        ErlangCallbackSpec spec = pointer.getElement();
        assert spec != null;
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        if (name == null || file.getFunction(name, arity) != null) continue;

        PsiElement lastChild = file.getLastChild();
        int textOffset = lastChild == null ? 0 : lastChild.getTextRange().getEndOffset();
        String newFunction = "\n\n" + name + "(" + createVariableList(spec) + ") ->\n" +
                             ErlangCreateFunctionQuickFix.FUNCTION_BODY_DEFAULT_TEXT;
        document.insertString(textOffset, newFunction);
        manager.commitDocument(document);
        CodeStyleManager.getInstance(project).reformatText(file, textOffset, textOffset + newFunction.length());
      }
    }

    private void exportAddedCallbackImplementations(@NotNull Project project, @NotNull ErlangFile file) {
      for (SmartPsiElementPointer<ErlangCallbackSpec> pointer : myCallbackSpecs) {
        ErlangCallbackSpec spec = pointer.getElement();
        assert spec != null;
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        ErlangFunction function = name != null ? file.getFunction(name, arity) : null;
        if (function != null && !function.isExported()) {
          new ErlangExportFunctionFix(function).invoke(project, file, null, function, null);
        }
      }
    }

    @NotNull
    private static String createVariableList(@NotNull ErlangCallbackSpec spec) {
      List<ErlangType> arguments = ErlangPsiImplUtil.getCallBackSpecArguments(spec);
      if (arguments.isEmpty()) return "";

      StringBuilder variables = new StringBuilder();
      String separator = ", ";
      for (ErlangType type : arguments) {
        ErlangQVar qVar = type.getQVar();
        variables.append(qVar != null ? qVar.getName() : "_").append(separator);
      }
      return variables.substring(0, variables.length() - separator.length());
    }
  }
}