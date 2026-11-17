/*
 * Copyright 2012-2026 Sergey Ignatov
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
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangCaseExpression;
import org.intellij.erlang.psi.ErlangCrClause;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangSingleClauseCaseInspection extends ErlangInspectionBase {
  @Override
  protected @NotNull ErlangVisitor buildErlangVisitor(@NotNull ProblemsHolder holder,
                                                      @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitCaseExpression(@NotNull ErlangCaseExpression o) {
        List<ErlangCrClause> clauses = o.getCrClauseList();
        if (clauses.size() == 1 && !mayHaveClausesFromMacro(clauses.get(0))) {
          registerProblem(holder, o.getCase(), "'case' expression has only one clause");
        }
      }
    };
  }

  // A macro call that the parser could not fit into the clause may expand to more clauses, as in
  // `case X of ?CLAUSES end` or `case X of a -> 1 ?MORE end`.
  private static boolean mayHaveClausesFromMacro(@NotNull ErlangCrClause clause) {
    return PsiTreeUtil.findChildOfType(clause, PsiErrorElement.class) != null &&
           SyntaxTraverser.psiTraverser(clause).traverse()
             .filter(e -> PsiUtilCore.getElementType(e) == ErlangTypes.ERL_QMARK)
             .isNotEmpty();
  }
}
