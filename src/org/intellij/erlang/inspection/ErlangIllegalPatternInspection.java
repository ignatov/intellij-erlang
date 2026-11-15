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
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

public class ErlangIllegalPatternInspection extends ErlangInspectionBase {
  private static final String ERROR = "Illegal pattern";
  private static final Set<String> STRING_MACROS = Set.of("MODULE_STRING", "FILE");
  private static final Set<String> INTEGER_MACROS = Set.of("LINE", "FUNCTION_ARITY");

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

  private static class PatternInspector extends ErlangRecursiveVisitor {
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

    private static void checkExpressionAndReport(@NotNull PsiElement element,
                                                 @NotNull final ProblemsHolder problemsHolder) {
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
          ErlangExpression left = o.getLeft();
          if (left instanceof ErlangColonQualifiedExpression qualified && isCatchClausePattern(o)) {
            List<ErlangExpression> classAndPrefix = qualified.getExpressionList();
            checkExpressionAndReport(classAndPrefix.get(0), problemsHolder);
            left = classAndPrefix.size() > 1 ? classAndPrefix.get(1) : null;
          }
          if (o.getOpPlusPlus() != null && isListPrefix(left) && o.getRight() != null && !isInMapKey(o)) {
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

        @Override
        public void visitCatchExpression(@NotNull ErlangCatchExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitFunExpression(@NotNull ErlangFunExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitAnonymousCallExpression(@NotNull ErlangAnonymousCallExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitIfExpression(@NotNull ErlangIfExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitPrefixExpression(@NotNull ErlangPrefixExpression o) {
          registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitGenericFunctionCallExpression(@NotNull ErlangGenericFunctionCallExpression o) {
          if (!(o.getParent() instanceof ErlangBinElement && isMacroCall(o))) {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitRecordExpression(@NotNull ErlangRecordExpression o) {
          if (o.getFirstChild() != o.getRadix()) {
            registerProblem(problemsHolder, o, ERROR);
          }
        }
      });
    }
  }

  private static boolean isMacroCall(@NotNull ErlangGenericFunctionCallExpression o) {
    PsiElement name = PsiTreeUtil.skipWhitespacesAndCommentsBackward(o.getArgumentList());
    return name instanceof ErlangMacros || name instanceof ErlangQAtom && ((ErlangQAtom) name).getMacros() != null;
  }

  // In a catch clause, Class:Prefix ++ Tail is parsed as (Class:Prefix) ++ Tail
  private static boolean isCatchClausePattern(@NotNull ErlangListOpExpression o) {
    PsiElement parent = o.getParent();
    return parent instanceof ErlangArgumentDefinition && parent.getParent() instanceof ErlangTryClause;
  }

  // A map key in a pattern is a guard expression, where ++ and -- are not allowed
  private static boolean isInMapKey(@NotNull PsiElement o) {
    PsiElement child = o;
    for (PsiElement parent = o.getParent(); parent != null; child = parent, parent = parent.getParent()) {
      if (parent instanceof ErlangArgumentDefinition || parent instanceof ErlangAssignmentExpression) return false;
      if (parent instanceof ErlangMapEntry entry) {
        List<ErlangExpression> keyAndValue = entry.getExpressionList();
        return keyAndValue.size() == 2 && keyAndValue.get(0) == child;
      }
    }
    return false;
  }

  // A predefined macro of a known kind may stand only where its kind may; any other macro may expand to anything
  private static boolean mayExpandTo(@NotNull ErlangMacros macros, @NotNull Set<String> kind) {
    String name = macros.getMacrosName().getText();
    return !ErlangPsiImplUtil.KNOWN_MACROS.contains(name) || kind.contains(name);
  }

  private static boolean isMacro(@Nullable ErlangExpression o, @NotNull Set<String> kind) {
    if (o instanceof ErlangFunctionCallExpression) return ((ErlangFunctionCallExpression) o).getQAtom().getMacros() != null;
    if (o instanceof ErlangGenericFunctionCallExpression call) return call.getColon() == null && isMacroCall(call);
    return o instanceof ErlangMaxExpression && isStringsAndMacros((ErlangMaxExpression) o, false, kind);
  }

  private static boolean isStringsAndMacros(@NotNull ErlangMaxExpression o, boolean allowStrings, @NotNull Set<String> kind) {
    for (PsiElement child = o.getFirstChild(); child != null; child = child.getNextSibling()) {
      if (child instanceof PsiWhiteSpace || child instanceof PsiComment || allowStrings && child instanceof ErlangStringLiteral) {
        continue;
      }
      ErlangMacros macros = child instanceof ErlangQAtom ? ((ErlangQAtom) child).getMacros() : child instanceof ErlangMacros ? (ErlangMacros) child : null;
      if (macros == null || !mayExpandTo(macros, kind)) return false;
    }
    return true;
  }

  private static boolean isCharOrIntegerOrMacro(@Nullable ErlangExpression o) {
    while (o instanceof ErlangParenthesizedExpression) o = ((ErlangParenthesizedExpression) o).getExpression();
    if (o instanceof ErlangMaxExpression && (((ErlangMaxExpression) o).getChar() != null || ((ErlangMaxExpression) o).getInteger() != null)) {
      return true;
    }
    return isMacro(o, INTEGER_MACROS);
  }

  // The left operand of ++ in a pattern: a string, or a list of characters and integers ending in [] or such a prefix (erl_lint)
  private static boolean isListPrefix(@Nullable ErlangExpression o) {
    while (o instanceof ErlangParenthesizedExpression) o = ((ErlangParenthesizedExpression) o).getExpression();
    if (o instanceof ErlangStringLiteral || isMacro(o, STRING_MACROS)) return true;
    if (o instanceof ErlangMaxExpression) return isStringsAndMacros((ErlangMaxExpression) o, true, STRING_MACROS);
    if (!(o instanceof ErlangListExpression list)) return false;
    List<ErlangExpression> expressions = list.getExpressionList();
    int elements = list.getOpOr() != null ? expressions.size() - 1 : expressions.size();
    for (int i = 0; i < elements; i++) {
      if (!isCharOrIntegerOrMacro(expressions.get(i))) return false;
    }
    return elements == expressions.size() || isListPrefix(expressions.get(elements));
  }

  private static class ConstantExpressionChecker extends ErlangRecursiveVisitor {
    private boolean myIsNonConstant;

    @Override
    public void visitQVar(@NotNull ErlangQVar o) {
      myIsNonConstant = true;
    }

    @Override
    public void visitQAtom(@NotNull ErlangQAtom o) {
      if (o.getMacros() == null) {
        myIsNonConstant = true;
      }
    }

    @Override
    public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
      if (o.getQAtom().getMacros() == null) {
        myIsNonConstant = true;
      }
    }

    @Override
    public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
      myIsNonConstant = true;
    }

    @Override
    public void visitListExpression(@NotNull ErlangListExpression o) {
      myIsNonConstant = true;
    }

    @Override
    public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
      myIsNonConstant = true;
    }

    @Override
    public void visitBinaryExpression(@NotNull ErlangBinaryExpression o) {
      myIsNonConstant = true;
    }

    public boolean isNonConstant() {
      return myIsNonConstant;
    }
  }
}

