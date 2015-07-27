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
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.Map;

public class ErlangUndefinedCallbackFunctionInspection extends ErlangInspectionBase {
  public static final String FIX_MESSAGE = "Implement and export all callbacks";

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiElement module = moduleRef != null ? moduleRef.getReference().resolve() : null;
      ErlangFile behaviourFile = module instanceof ErlangModule ? (ErlangFile) module.getContainingFile() : null;
      PsiElement toHighlight = behaviour.getParLeft() != null ? behaviour.getParLeft().getNextSibling() : null;
      if (behaviourFile == null || toHighlight == null) continue;

      List<ErlangCallbackSpec> undefinedCallbacks = ContainerUtil.newArrayList();
      Map<String, ErlangCallbackSpec> callbackMap = behaviourFile.getCallbackMap();
      for (ErlangCallbackSpec spec : callbackMap.values()) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        ErlangFunction function = name != null ? file.getFunction(name, arity) : null;
        if (function == null || !function.isExported()) {
          undefinedCallbacks.add(spec);
        }
      }
      if (!undefinedCallbacks.isEmpty()) {
        boolean multiple = undefinedCallbacks.size() != 1;
        StringBuilder builder = new StringBuilder();
        builder.append("Undefined or non-exported callback function").append(multiple ? "s" : "").append(": ");
        for (ErlangCallbackSpec spec : undefinedCallbacks) {
          builder.append("'").append(ErlangPsiImplUtil.createFunctionPresentationFromCallbackSpec(spec)).append("', ");
        }
        String message = builder.substring(0, builder.length() - 2);
        registerProblem(problemsHolder, toHighlight, message, new MyLocalQuickFixBase(undefinedCallbacks));
      }
    }
  }

  private static class MyLocalQuickFixBase extends LocalQuickFixBase {
    @NotNull
    private final Collection<ErlangCallbackSpec> myCallbackSpecs;

    protected MyLocalQuickFixBase(@NotNull Collection<ErlangCallbackSpec> callbackSpecs) {
      super(FIX_MESSAGE);
      myCallbackSpecs = callbackSpecs;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      PsiFile file = problemDescriptor.getPsiElement().getContainingFile();
      ErlangFile erlangFile = file instanceof ErlangFile ? (ErlangFile) file : null;
      if (erlangFile != null) {
        addCallbackImplementations(project, erlangFile);
        exportAddedCallbackImplementations(project, erlangFile);
      }
    }

    private void addCallbackImplementations(@NotNull Project project, @NotNull ErlangFile file) {
      PsiDocumentManager manager = PsiDocumentManager.getInstance(project);
      Document document = manager.getDocument(file);
      if (document == null) return;

      for (ErlangCallbackSpec spec : myCallbackSpecs) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallbackSpecArity(spec);
        if (name == null || file.getFunction(name, arity) != null) continue;

        PsiElement lastChild = file.getLastChild();
        int textOffset = lastChild == null ? 0 : lastChild.getTextRange().getEndOffset();
        String newFunction = "\n" + name + "(" + createVariableList(spec) + ") ->\n erlang:error(not_implemented).\n";
        document.insertString(textOffset, newFunction);
        manager.commitDocument(document);
        CodeStyleManager.getInstance(project).reformatText(file, textOffset, textOffset + newFunction.length());
      }
    }

    private void exportAddedCallbackImplementations(@NotNull Project project, @NotNull ErlangFile file) {
      for (ErlangCallbackSpec spec : myCallbackSpecs) {
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
      StringBuilder variables = new StringBuilder();
      for (ErlangType type : arguments) {
        ErlangQVar qVar = type.getQVar();
        variables.append(qVar != null ? qVar.getName() : "_").append(", ");
      }
      return variables.substring(0, variables.length() - 2);
    }
  }
}