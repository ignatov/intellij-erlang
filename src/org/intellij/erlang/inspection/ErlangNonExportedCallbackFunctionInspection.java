/*
 * Copyright 2012-2015 Sergey Ignatov
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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class ErlangNonExportedCallbackFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    //noinspection unchecked
    ErlangCompositeElement warningHolder = PsiTreeUtil.getChildOfAnyType(file, ErlangAttribute.class, ErlangModule.class);
    if (warningHolder == null) return;

    List<ErlangFunction> nonExportedFunctions = new LinkedList<ErlangFunction>();
    List<Pair<String, String>> namesAndNames = new LinkedList<Pair<String, String>>();

    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ErlangModuleRef moduleRef = behaviour.getModuleRef();
      PsiElement resolvedModule = moduleRef != null ? moduleRef.getReference().resolve() : null;

      if (resolvedModule instanceof ErlangModule) {
        PsiFile containingFile = resolvedModule.getContainingFile();
        if (containingFile instanceof ErlangFile) {
          Map<String, ErlangCallbackSpec> callbackMap = ((ErlangFile) containingFile).getCallbackMap();
          for (Map.Entry<String, ErlangCallbackSpec> entry : callbackMap.entrySet()) {
            String fullName = entry.getKey();
            List<String> splitName = StringUtil.split(fullName, "/");
            if (splitName.size() != 2) continue;
            ErlangFunction function = file.getFunction(splitName.get(0), StringUtil.parseInt(splitName.get(1), -1));
            if (function != null && !file.getExportedFunctions().contains(function)) {
              nonExportedFunctions.add(function);
              String fileName = function.getContainingFile().getName();
              namesAndNames.add(Pair.create(fullName, fileName));
            }
          }
        }
      }
    }

    if (!nonExportedFunctions.isEmpty()) {
      boolean multiple = nonExportedFunctions.size() != 1;
      String message = "Non-exported callback function" + (multiple ? "s" : "") + ": ";
      boolean first = true;
      for (Pair<String, String> pair : namesAndNames) {
        if (first) first = false;
        else message += ", ";
        message += "'" + pair.first + "'";
        message += " (behaviour " + pair.second + ")";
      }
      problemsHolder.registerProblem(warningHolder, message, new MyLocalQuickFixBase(nonExportedFunctions));
    }
  }

  private static class MyLocalQuickFixBase extends LocalQuickFixBase {
    @NotNull
    private final Collection<ErlangFunction> myNonExportedFunctions;

    protected MyLocalQuickFixBase(@NotNull Collection<ErlangFunction> functions) {
      super("Export all callbacks");
      myNonExportedFunctions = functions;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      PsiFile file = problemDescriptor.getPsiElement().getContainingFile();
      if (file instanceof ErlangFile) {
        for (ErlangFunction function : myNonExportedFunctions) {
          new ErlangExportFunctionFix(function).invoke(project, file, null, function, null);
        }
      }
    }
  }
}
