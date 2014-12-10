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

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangImportFunction;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFix;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFromImportFix;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

public class ErlangDefiningImportedFunctionInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    return !file.getName().endsWith(ErlangFileType.HEADER.getDefaultExtension()) && !file.isExportedAll();
  }

  @NotNull
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFile(final PsiFile file) {
        Set<String> importedFunctionNames = ContainerUtil.newHashSet();
        for (ErlangImportFunction f : ((ErlangFile) file).getImportedFunctions()) {
          importedFunctionNames.add(ErlangPsiImplUtil.createFunctionPresentation(f));
        }
        for (ErlangFunction function : ((ErlangFile) file).getFunctions()) {
          String fullName = ErlangPsiImplUtil.createFunctionPresentation(function);
          if (importedFunctionNames.contains(fullName)) {
            holder.registerProblem(function.getNameIdentifier(), "Defining imported function " + "'" + fullName + "'",
              ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
              new ErlangRemoveFunctionFix(),
              new ErlangRemoveFunctionFromImportFix());
          }
        }
      }
    };
  }

}
