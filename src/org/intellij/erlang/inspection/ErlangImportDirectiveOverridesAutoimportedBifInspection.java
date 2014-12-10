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
import com.intellij.psi.PsiReference;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFromImportFix;
import org.intellij.erlang.quickfixes.ErlangSpecifyModulePrefixFix;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

public class ErlangImportDirectiveOverridesAutoimportedBifInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || release.isNewerThan(ErlangSdkRelease.V_R14A);
  }

  @NotNull
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitImportFunction(@NotNull ErlangImportFunction o) {
        PsiReference reference = o.getReference();
        if (!(reference instanceof ErlangFunctionReferenceImpl) || reference.resolve() == null) return;
        ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;
        String name = r.getName();
        int arity = r.getArity();
        ErlangBifDescriptor bifDescriptor = ErlangBifTable.getBif("erlang", name, arity);
        if (bifDescriptor == null || !bifDescriptor.isAutoImported())  return;
        holder.registerProblem(o, "Import directive overrides pre R14 auto-imported BIF " + "'" + r.getSignature() + "'",
          ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
          new ErlangRemoveFunctionFromImportFix());
      }
    };
  }

}