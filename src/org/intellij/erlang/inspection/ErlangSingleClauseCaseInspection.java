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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.JBIterable;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
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

  // A macro call may expand to more clauses, which the parser cannot see. Either the parser could not fit the
  // call into the clause, as in `case X of ?CLAUSES end`, or the macro body has a `;` at the top level, as in
  // `case X of a -> ?REST end` with `-define(REST, ok; _ -> error)`. Nested expressions with their own clauses
  // are not searched.
  private static boolean mayHaveClausesFromMacro(@NotNull ErlangCrClause clause) {
    JBIterable<PsiElement> elements = SyntaxTraverser.psiTraverser(clause)
      .expand(e -> e == clause || !(e instanceof ErlangClauseOwner || e instanceof ErlangIfExpression || e instanceof ErlangFunExpression))
      .traverse();
    boolean hasMacroCall = elements.filter(e -> PsiUtilCore.getElementType(e) == ErlangTypes.ERL_QMARK).isNotEmpty();
    if (!hasMacroCall) return false;
    if (elements.filter(PsiErrorElement.class).isNotEmpty()) return true;
    // In a guard, `;` separates alternatives, not clauses.
    ErlangClauseGuard guard = clause.getClauseGuard();
    return elements.filter(ErlangMacros.class)
      .filter(m -> !PsiTreeUtil.isAncestor(guard, m, false))
      .filter(ErlangSingleClauseCaseInspection::bodyHasTopLevelSemicolon)
      .isNotEmpty();
  }

  private static boolean bodyHasTopLevelSemicolon(@NotNull ErlangMacros macros) {
    PsiReference reference = macros.getReference();
    ErlangMacrosDefinition definition = ObjectUtils.tryCast(ErlangMacroResolve.firstTarget(reference), ErlangMacrosDefinition.class);
    ErlangMacrosBody body = definition != null ? definition.getMacrosBody() : null;
    return body != null && SyntaxTraverser.psiApi().children(body).filter(e -> PsiUtilCore.getElementType(e) == ErlangTypes.ERL_SEMI).isNotEmpty();
  }
}
