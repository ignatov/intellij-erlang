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
import com.intellij.openapi.util.text.StringUtil;
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
  private static final Set<String> INTEGER_MACROS = Set.of("LINE", "FUNCTION_ARITY", "OTP_RELEASE");
  private static final Set<String> FUNCTION_MACROS = Set.of("FUNCTION_NAME", "FUNCTION_ARITY");

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
      checkExpressionAndReport(element, element, problemsHolder);
    }

    // The pattern is the whole checked expression, which tells whether a part of it is in a guard expression
    private static void checkExpressionAndReport(@NotNull PsiElement part,
                                                 @NotNull PsiElement element,
                                                 @NotNull final ProblemsHolder problemsHolder) {
      part.accept(new ErlangRecursiveVisitor() {

        private void registerProblemIfContainsNonConstant(@NotNull ErlangExpression o) {
          ConstantExpressionChecker checker = new ConstantExpressionChecker();
          o.accept(checker);
          if (checker.isNonConstant()) {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitAdditiveExpression(@NotNull ErlangAdditiveExpression o) {
          if (isInSizeOrMapKey(o, element)) super.visitAdditiveExpression(o);
          else registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitMultiplicativeExpression(@NotNull ErlangMultiplicativeExpression o) {
          if (isInSizeOrMapKey(o, element)) super.visitMultiplicativeExpression(o);
          else registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitOrelseExpression(@NotNull ErlangOrelseExpression o) {
          if (isInSizeOrMapKey(o, element)) super.visitOrelseExpression(o);
          else registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitAndalsoExpression(@NotNull ErlangAndalsoExpression o) {
          if (isInSizeOrMapKey(o, element)) super.visitAndalsoExpression(o);
          else registerProblemIfContainsNonConstant(o);
        }

        @Override
        public void visitListOpExpression(@NotNull ErlangListOpExpression o) {
          ErlangExpression left = o.getLeft();
          boolean classAndMacroCall = false;
          if (left instanceof ErlangColonQualifiedExpression qualified && isCatchClausePattern(o)) {
            List<ErlangExpression> classAndPrefix = qualified.getExpressionList();
            checkExpressionAndReport(classAndPrefix.get(0), problemsHolder);
            left = classAndPrefix.size() > 1 ? classAndPrefix.get(1) : null;
          }
          // Class:?MACRO(Args) is parsed as a remote call
          else if (left instanceof ErlangGlobalFunctionCallExpression call && isCatchClausePattern(o)) {
            left = call.getFunctionCallExpression();
          }
          else if (left instanceof ErlangGenericFunctionCallExpression call && call.getColon() != null && isCatchClausePattern(o)) {
            classAndMacroCall = isMacroCall(call);
          }
          if (o.getOpPlusPlus() != null && (classAndMacroCall || isListPrefix(left)) && o.getRight() != null &&
              !isInGuardExpression(o, element)) {
            checkExpressionAndReport(o.getRight(), problemsHolder);
          }
          else {
            registerProblem(problemsHolder, o, ERROR);
          }
        }

        @Override
        public void visitAssignmentExpression(@NotNull ErlangAssignmentExpression o) {
          // A match is not a guard expression; report it unless a part of it is reported
          if (isInSizeOrMapKey(o, element) && !hasProblems(o, element, problemsHolder)) {
            registerProblem(problemsHolder, o, ERROR);
          }
          else {
            super.visitAssignmentExpression(o);
          }
        }

        @Override
        public void visitCompOpExpression(@NotNull ErlangCompOpExpression o) {
          if (isInSizeOrMapKey(o, element)) super.visitCompOpExpression(o);
          else registerProblem(problemsHolder, o, ERROR);
        }

        @Override
        public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
          boolean macro = o.getQAtom().getMacros() != null;
          if (isInSizeOrMapKey(o, element) && (macro || isGuardBifCall(o))) super.visitFunctionCallExpression(o);
          else if (!macro) {
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
          if (isInSizeOrMapKey(o, element)) super.visitPrefixExpression(o);
          else registerProblemIfContainsNonConstant(o);
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
          else if (isInGuardExpression(o, element)) {
            reportListOps(o, problemsHolder);
          }
        }
      });
    }

    private static boolean hasProblems(@NotNull PsiElement o, @NotNull PsiElement pattern, @NotNull ProblemsHolder holder) {
      ProblemsHolder tempHolder = new ProblemsHolder(holder.getManager(), holder.getFile(), holder.isOnTheFly());
      for (PsiElement child = o.getFirstChild(); child != null; child = child.getNextSibling()) {
        checkExpressionAndReport(child, pattern, tempHolder);
      }
      return !tempHolder.getResults().isEmpty();
    }

    // Only ++ and -- are checked in the fields of a record in a guard expression; other guard expressions are allowed there
    private static void reportListOps(@NotNull PsiElement element, @NotNull ProblemsHolder problemsHolder) {
      element.accept(new ErlangRecursiveVisitor() {
        @Override
        public void visitListOpExpression(@NotNull ErlangListOpExpression o) {
          registerProblem(problemsHolder, o, ERROR);
        }
      });
    }
  }

  private static boolean isMacroCall(@NotNull ErlangGenericFunctionCallExpression o) {
    PsiElement name = PsiTreeUtil.skipWhitespacesAndCommentsBackward(o.getArgumentList());
    return name instanceof ErlangMacros || name instanceof ErlangQAtom && ((ErlangQAtom) name).getMacros() != null;
  }

  // In a catch clause, Class:Prefix ++ Tail [= Pattern] is parsed as ((Class:Prefix) ++ Tail) [= Pattern]
  private static boolean isCatchClausePattern(@NotNull ErlangListOpExpression o) {
    PsiElement child = o;
    PsiElement parent = o.getParent();
    while (parent instanceof ErlangAssignmentExpression assignment && assignment.getLeft() == child) {
      child = parent;
      parent = parent.getParent();
    }
    return parent instanceof ErlangArgumentDefinition && parent.getParent() instanceof ErlangTryClause;
  }

  // A binary segment and a map key in a pattern are guard expressions, where ++ and -- are not allowed
  private static boolean isInGuardExpression(@NotNull PsiElement o, @NotNull PsiElement pattern) {
    for (PsiElement child = o, parent = o.getParent(); child != pattern && parent != null; child = parent, parent = parent.getParent()) {
      if (parent instanceof ErlangBinElement) return true;
      if (parent instanceof ErlangMapEntry entry) {
        List<ErlangExpression> keyAndValue = entry.getExpressionList();
        if (keyAndValue.size() == 2 && keyAndValue.get(0) == child) return true;
      }
    }
    return false;
  }

  // The size of a binary segment and a map key in a pattern are guard expressions, which may compute with bound variables
  private static boolean isInSizeOrMapKey(@NotNull PsiElement o, @NotNull PsiElement pattern) {
    for (PsiElement child = o, parent = o.getParent(); child != pattern && parent != null; child = parent, parent = parent.getParent()) {
      if (parent instanceof ErlangBinElement element) {
        List<ErlangExpression> valueAndSize = element.getExpressionList();
        if (valueAndSize.size() == 2 && valueAndSize.get(1) == child) return true;
      }
      if (parent instanceof ErlangMapEntry entry && entry.getMatch() != null) {
        List<ErlangExpression> keyAndValue = entry.getExpressionList();
        if (keyAndValue.size() == 2 && keyAndValue.get(0) == child) return true;
      }
    }
    return false;
  }

  private static boolean isGuardBifCall(@NotNull ErlangFunctionCallExpression o) {
    String name = o.getName();
    int arity = o.getArgumentList().getExpressionList().size();
    String presentation = ErlangPsiImplUtil.createFunctionPresentation(name, arity);
    if (o.getParent() instanceof ErlangGlobalFunctionCallExpression global) {
      return ErlangIllegalGuardInspection.isGuardBif(ErlangPsiImplUtil.getName(global.getModuleRef().getQAtom()), presentation);
    }
    return ((ErlangFile) o.getContainingFile()).getFunction(name, arity) == null &&
           ErlangIllegalGuardInspection.isGuardBif(null, presentation);
  }

  // A predefined macro of a known kind may stand only where its kind may; any other macro may expand to anything
  private static boolean mayExpandTo(@NotNull ErlangMacros macros, @NotNull Set<String> kind) {
    String name = getPredefinedMacroName(macros.getMacrosName());
    return name == null || kind.contains(name);
  }

  // epp accepts the quoted spelling of the predefined macros, except ?'FUNCTION_NAME' and ?'FUNCTION_ARITY'
  @Nullable
  private static String getPredefinedMacroName(@NotNull ErlangMacrosName macrosName) {
    boolean quoted = macrosName.getAtom() != null;
    String name = quoted ? StringUtil.unquoteString(macrosName.getText(), '\'') : macrosName.getText();
    if (quoted && FUNCTION_MACROS.contains(name)) return null;
    return ErlangPsiImplUtil.KNOWN_MACROS.contains(name) ? name : null;
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

