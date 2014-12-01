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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangRenameVariableFix;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangUnusedVariableInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull final ProblemsHolder problemsHolder) {
    for (ErlangFunction function : file.getFunctions()) {
      for (final ErlangFunctionClause functionClause : function.getFunctionClauseList()) {
        functionClause.accept(new ErlangRecursiveVisitor() {
          @Override
          public void visitQVar(@NotNull ErlangQVar o) {
            if (isForceSkipped(o) || !inLeftPartOfAssignment(o) && (!inArgumentDefinition(o) || inArgumentList(o))) return;

            PsiReference reference = o.getReference();
            PsiElement resolve = reference != null ? reference.resolve() : null;
            if (resolve != null) return;

            Query<PsiReference> search = ReferencesSearch.search(o, new LocalSearchScope(functionClause));
            for (PsiReference ref : search) {
              PsiElement element = ref.getElement();
              if (ErlangPsiImplUtil.fromTheSameCaseExpression(o, element)) {
                PsiReference reference1 = element.getReference();
                if (reference1 == null || reference1.resolve() == null) continue;
              }
              return;
            }

            registerProblem(problemsHolder, o, "Unused variable " + "'" + o.getText() + "'", null,
              ProblemHighlightType.LIKE_UNUSED_SYMBOL, new ErlangRenameVariableFix());
          }
        });
      }
    }
  }
}
