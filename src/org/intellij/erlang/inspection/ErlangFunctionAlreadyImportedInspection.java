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

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFromImportFixBase;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ErlangFunctionAlreadyImportedInspection extends ErlangInspectionBase {
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    Map<String, String> alreadyImported = new HashMap<>();
    for (ErlangAttribute attribute : file.getAttributes()) {
      ErlangImportFunctions importFunctions = attribute.getImportDirective() != null ? attribute.getImportDirective().getImportFunctions() : null;
      if (importFunctions == null) continue;
      ErlangModuleRef moduleRef = attribute.getImportDirective().getModuleRef();
      if (moduleRef == null) continue;
      processImportDirective(importFunctions, ErlangPsiImplUtil.getName(moduleRef.getQAtom()), alreadyImported, problemsHolder);
    }
  }

  private static void processImportDirective(@NotNull ErlangImportFunctions importFunctions,
                                             @NotNull String importFrom,
                                             @NotNull Map<String, String> alreadyImported,
                                             @NotNull ProblemsHolder problemsHolder) {
    Set<String> justImported = new HashSet<>();
    LocalQuickFix fix = new ErlangRemoveFunctionFromImportFixBase.ErlangRemoveFunctionFromImportFix();
    ProblemHighlightType severity = ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
    for (ErlangImportFunction f : importFunctions.getImportFunctionList()) {
      String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(f);
      if (alreadyImported.containsKey(functionPresentation)) {
        String text = "Function '" + functionPresentation + "' already imported from '" + alreadyImported.get(functionPresentation) + "'";
        problemsHolder.registerProblem(f, text, severity, fix);
      }
      if (justImported.contains(functionPresentation)) {
        String text = "Duplicate import of function '" + functionPresentation + "'";
        problemsHolder.registerProblem(f, text, ProblemHighlightType.WEAK_WARNING, fix);
      }
      justImported.add(functionPresentation);
    }
    for (String name : justImported) {
      alreadyImported.put(name, importFrom);
    }
  }
}