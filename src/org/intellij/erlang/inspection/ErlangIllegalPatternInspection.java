/*
 * Copyright 2012-2015 Sergey Ignatov
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
import org.jetbrains.annotations.NotNull;

public class ErlangIllegalPatternInspection extends ErlangInspectionBase {
  private static final String ERROR = "Illegal pattern";

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    for (ErlangFunction function : file.getFunctions()) {
      for (ErlangFunctionClause clause : function.getFunctionClauseList()) {
        clause.getArgumentDefinitionList().accept(new PatternInspector(holder));
        ErlangClauseBody clauseBody = clause.getClauseBody();
        if (clauseBody != null) {
          clauseBody.accept(new PatternInspector(holder));
        }
      }
    }
  }

  private class PatternInspector extends ErlangRecursiveVisitor {
    private final ProblemsHolder myHolder;

    public PatternInspector(ProblemsHolder holder) {
      this.myHolder = holder;
    }

    @Override
    public void visitArgumentDefinition(@NotNull ErlangArgumentDefinition o) {
      checkExpression(o);
    }

    @Override
    public void visitAssignmentExpression(@NotNull ErlangAssignmentExpression o) {
      ErlangExpression leftPart = o.getLeft();
      checkExpression(leftPart);
      ErlangExpression rightPart = o.getRight();
      if (rightPart != null) {
        rightPart.accept(this);
      }
    }

    private void checkExpression(@NotNull PsiElement element) {
      if (element instanceof ErlangListOpExpression) {
        visitGlobalListOpExpression((ErlangListOpExpression) element);
      }
      else {
        checkExpressionAndReport(element, myHolder);
      }
    }

    private void visitGlobalListOpExpression(@NotNull ErlangListOpExpression o) {
      ProblemsHolder tempHolder = new ProblemsHolder(myHolder.getManager(), myHolder.getFile(), myHolder.isOnTheFly());
      checkExpressionAndReport(o, tempHolder);
      if (!tempHolder.getResults().isEmpty()) {
        registerProblem(myHolder, o, ERROR);
      }
    }

    private void checkExpressionAndReport(@NotNull PsiElement element, @NotNull final ProblemsHolder problemsHolder) {
      element.accept(new ErlangRecursiveVisitor() {

        private void registerProblemIfContainsNonConstant(@NotNull ErlangExpression o) {
          ConstantExpressionChecker checker = new ConstantExpressionChecker();
          o.accept(checker);
          if (checker.isNonConstant()) {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitAdditiveExpression(@NotNull ErlangAdditiveExpression o) {
          registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitMultiplicativeExpression(@NotNull ErlangMultiplicativeExpression o) {
          registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitOrelseExpression(@NotNull ErlangOrelseExpression o) {
          registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitAndalsoExpression(@NotNull ErlangAndalsoExpression o) {
          registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitListOpExpression(@NotNull ErlangListOpExpression o) {
          if (o.getLeft() instanceof ErlangStringLiteral && o.getRight() != null) {
            checkExpressionAndReport(o.getRight(), problemsHolder);
          }
          else {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitCompOpExpression(@NotNull ErlangCompOpExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
          if (o.getQAtom().getMacros() == null) {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitCaseExpression(@NotNull ErlangCaseExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitReceiveExpression(@NotNull ErlangReceiveExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitTryExpression(@NotNull ErlangTryExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitListComprehension(@NotNull ErlangListComprehension o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitBeginEndExpression(@NotNull ErlangBeginEndExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitSendExpression(@NotNull ErlangSendExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }
      });
    }
  }

  private static class ConstantExpressionChecker extends ErlangRecursiveVisitor {
    private boolean myIsNonConstant;

    @Override
    public void visitQVar(@NotNull ErlangQVar o) {
      myIsNonConstant = true;
    }

    @Override
    public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
      if (o.getQAtom().getMacros() == null) {
        myIsNonConstant = true;
      }
    }

    public boolean isNonConstant() {
      return myIsNonConstant;
    }
  }
}

