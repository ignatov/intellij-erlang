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
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangIntroduceRecordFieldFix;
import org.jetbrains.annotations.NotNull;

public class ErlangUnresolvedRecordFieldInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitRecordField(@NotNull ErlangRecordField o) {
        super.visitRecordField(o);
        ErlangRecordExpression recordExpression = PsiTreeUtil.getParentOfType(o, ErlangRecordExpression.class);
        if (recordExpression != null) {
          PsiReference reference = recordExpression.getReferenceInternal();
          if (reference != null && reference.resolve() == null) {
            return;
          }
        }

        PsiReference reference = o.getReference();
        if (reference == null || reference.resolve() == null) {
          ErlangQAtom atom = o.getFieldNameAtom();
          if (atom == null || atom.getMacros() != null) return;
          problemsHolder.registerProblem(atom, "Unresolved record field " + "'" + atom.getText() + "'", new ErlangIntroduceRecordFieldFix());
        }
      }
    });
  }
}