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
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangIntroduceRecordFix;
import org.jetbrains.annotations.NotNull;

public class ErlangUnresolvedRecordInspection extends ErlangInspectionBase {
  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitRecordRef(@NotNull ErlangRecordRef o) {
        if (o.getQAtom().getMacros() != null) return;
        process(o, holder);
      }

      @Override
      public void visitQAtom(@NotNull ErlangQAtom o) {
        if (ErlangPsiImplUtil.secondAtomInIsRecord().accepts(o, new ProcessingContext())) {
          process(o, holder);
        }
      }
    };
  }

  private void process(@NotNull PsiElement o, @NotNull ProblemsHolder problemsHolder) {
    PsiReference ref = o.getReference();
    if (ref == null || ref.resolve() == null) {
      registerProblem(problemsHolder, o, "Unresolved record " + "'" + o.getText() + "'", new ErlangIntroduceRecordFix());
    }
  }
}
