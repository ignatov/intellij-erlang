/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class ErlangUndefinedCallbackFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    //noinspection unchecked
    ErlangCompositeElement warningHolder = PsiTreeUtil.getChildOfAnyType(file, ErlangAttribute.class, ErlangModule.class);
    if (warningHolder == null) return;

    List<ErlangCallbackSpec> toOverwrite = new LinkedList<ErlangCallbackSpec>();
    List<Pair<String, String >> namesAndNames = new LinkedList<Pair<String, String>>();

    for (ErlangBehaviour behaviour : ((ErlangFile) file).getBehaviours()) {
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiReference reference = moduleRef != null ? moduleRef.getReference() : null;
      PsiElement resolve = reference != null ? reference.resolve() : null;

      if (resolve instanceof ErlangModule) {
        PsiFile containingFile = resolve.getContainingFile();
        if (containingFile instanceof ErlangFile) {
          Map<String, ErlangCallbackSpec> callbackMap = ((ErlangFile) containingFile).getCallbackMap();
          for (Map.Entry<String, ErlangCallbackSpec> entry : callbackMap.entrySet()) {
            String fullName = entry.getKey();
            List<String> split = StringUtil.split(fullName, "/");
            if (split.size() != 2) continue;
            ErlangFunction function = ((ErlangFile) file).getFunction(split.get(0), StringUtil.parseInt(split.get(1), -1));
            if (function == null) {
              ErlangCallbackSpec spec = entry.getValue();
              toOverwrite.add(spec);
              String fileName = spec.getContainingFile().getName();
              namesAndNames.add(Pair.create(fullName, fileName));
            }
          }
        }
      }
    }

    if (!toOverwrite.isEmpty()) {
      boolean multiple = toOverwrite.size() != 1;
      String message = "Undefined callback function" + (multiple ? "s" : "") + ": ";
      boolean first = true;
      for (Pair<String, String> pair : namesAndNames) {
        if (first) first = false;
        else message += ", ";
        message += "'" + pair.first + "'";
        message += " (behaviour " + pair.second + ")";
      }

      problemsHolder.registerProblem(warningHolder, message, new MyLocalQuickFixBase(toOverwrite));
    }
  }

  private static class MyLocalQuickFixBase extends LocalQuickFixBase {
    @NotNull
    private final Collection<ErlangCallbackSpec> myCallbackSpecs;

    protected MyLocalQuickFixBase(@NotNull Collection<ErlangCallbackSpec> callbackSpecs) {
      super("Implement all callbacks");
      myCallbackSpecs = callbackSpecs;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      PsiFile file = problemDescriptor.getPsiElement().getContainingFile();

      for (ErlangCallbackSpec spec : myCallbackSpecs) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        List<ErlangTopType> topTypeList = ErlangPsiImplUtil.getCallBackSpecArguments(spec);

        List<String> vars = new LinkedList<String>();
        for (ErlangTopType type : topTypeList) {
          ErlangQVar qVar = type.getQVar();
          vars.add(qVar != null ? qVar.getName() : "_");
        }

        PsiDocumentManager manager = PsiDocumentManager.getInstance(project);
        Document document = manager.getDocument(file);
        PsiElement lastChild = file.getLastChild();
        int textOffset = lastChild == null ? 0 : lastChild.getTextRange().getEndOffset();

        if (document == null) return;
        String join = StringUtil.join(vars, ", ");
        String newFunction = "\n" + name + "(" + join + ") ->\n erlang:error(not_implemented).\n";
        document.insertString(textOffset, newFunction);
        manager.commitDocument(document);
        CodeStyleManager.getInstance(project).reformatText(file, textOffset, textOffset + newFunction.length());
      }
    }
  }
}