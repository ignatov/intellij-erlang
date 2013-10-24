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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangIntroduceRecordFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author ignatov
 */
public class ErlangUnresolvedRecordInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitRecordExpression(@NotNull ErlangRecordExpression o) {
        if (o.getMacros() != null) return;
        processRef(o, o.getRecordRef(), problemsHolder);
        super.visitRecordExpression(o);
      }

      @Override
      public void visitRecordType(@NotNull ErlangRecordType o) {
        processRef(o, o.getRecordRef(), problemsHolder);
        super.visitRecordType(o);
      }
    });
  }

  private static void processRef(@NotNull PsiElement o, @Nullable ErlangRecordRef ref, @NotNull ProblemsHolder problemsHolder) {
    if (ref != null && ref.getQAtom().getMacros() != null) return;
    PsiReference reference = ref != null ? ref.getReference() : null;
    if (reference == null || reference.resolve() == null) {
      problemsHolder.registerProblem(ref != null ? ref : o,
        "Unresolved record " + "'" + (ref != null ? ref.getText() : "") + "'",
        new ErlangIntroduceRecordFix());
    }
  }
}
