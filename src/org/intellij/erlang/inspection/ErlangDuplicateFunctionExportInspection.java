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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangRemoveDuplicateFunctionExportFix;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangDuplicateFunctionExportInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    Set<String> exported = new HashSet<>();
    for (ErlangAttribute attribute : file.getAttributes()) {
      ErlangExport export = attribute.getExport();
      ErlangExportFunctions exportFunctions = export != null ? export.getExportFunctions() : null;
      if (exportFunctions == null) continue;
      List<ErlangExportFunction> list = exportFunctions.getExportFunctionList();
      for (ErlangExportFunction exportFunction : list) {
        PsiElement integer = exportFunction.getInteger();
        if (integer == null) continue;
        String s = ErlangPsiImplUtil.getExportFunctionName(exportFunction) + "/" + integer.getText();
        if (exported.contains(s)) {
          problemsHolder.registerProblem(exportFunction, "Function " + "'" + s + "' has been already exported.",
            new ErlangRemoveDuplicateFunctionExportFix());
        }
        else {
          exported.add(s);
        }
      }
    }
  }
}
