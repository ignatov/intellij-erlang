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

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangImportFunction;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFromImportFixBase;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

public class ErlangImportDirectiveOverridesAutoImportedBifInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || release.isNewerThan(ErlangSdkRelease.V_R14A);
  }

  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    for (ErlangImportFunction importFunction : file.getImportedFunctions()) {
      PsiReference reference = importFunction.getReference();
      if (!(reference instanceof ErlangFunctionReferenceImpl) || reference.resolve() == null) continue;
      ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;
      ErlangBifDescriptor bifDescriptor = ErlangBifTable.getBif("erlang", r.getName(), r.getArity());
      if (bifDescriptor == null || !bifDescriptor.isAutoImported())  continue;
      problemsHolder.registerProblem(importFunction,
        "Import directive overrides pre R14 auto-imported BIF '" + r.getSignature() + "'",
        ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
        new ErlangRemoveFunctionFromImportFixBase.ErlangRemoveFunctionFromImportFix());
    }
  }

}