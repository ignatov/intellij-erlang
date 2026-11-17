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

import com.intellij.codeInspection.*;
import com.intellij.codeInspection.util.IntentionFamilyName;
import com.intellij.codeInspection.util.IntentionName;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

//TODO add arity mismatch checks
public class ErlangHeadMismatchInspection extends ErlangInspectionBase implements DumbAware {
  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitFunction(@NotNull ErlangFunction function) {
        checkFunction(function, holder);
      }

      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression funExpression) {
        checkFunExpression(funExpression, holder);
      }
    };
  }

  private static void checkFunction(ErlangFunction function, ProblemsHolder problemsHolder) {
    String functionName = function.getName();
    List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
    if (clauses.size() <= 1) return;
    for (ErlangFunctionClause clause : clauses) {
      ErlangQAtom clauseHead = clause.getQAtom();
      if (clauseHead.getMacros() != null) return;
      String clauseSignature = ErlangPsiImplUtil.createFunctionClausePresentation(clause);
      String functionSignature = ErlangPsiImplUtil.createFunctionPresentation(function);

      if (!functionSignature.equals(clauseSignature)) {
        registerProblem(problemsHolder, clauseHead, "Head mismatch: should be '" + functionSignature + "'",
          new RenameFunctionClauseHeadQuickFix(functionName));
      }
    }
  }

  private static void checkFunExpression(ErlangFunExpression funExpression, ProblemsHolder problemsHolder) {
    ErlangFunClauses funClauses = funExpression.getFunClauses();
    List<ErlangFunClause> funClauseList = funClauses != null ? funClauses.getFunClauseList() : Collections.emptyList();
    if (funClauseList.size() <= 1) return;
    ErlangFunClause firstClause = funClauseList.getFirst();
    String firstClauseName = getFunExpressionClauseName(firstClause);
    for (ErlangFunClause funClause : funClauseList) {
      String funClauseName = getFunExpressionClauseName(funClause);
      if (!Objects.equals(firstClauseName, funClauseName)) {
        String problemDescription = firstClauseName == null ?
          "Head mismatch: named clause in an unnamed fun expression" :
          funClauseName == null ?
            "Head mismatch: unnamed clause in a named fun expression" :
            "Head mismatch: should be '" + firstClauseName + "'";
        PsiElement elementForRange = funClauseName != null ? funClause.getArgumentDefinition() : funClause.getArgumentDefinitionList();
        if (elementForRange != null) {
          TextRange range = TextRange.create(elementForRange.getStartOffsetInParent(),
            elementForRange.getStartOffsetInParent() + elementForRange.getTextLength());
          registerProblem(problemsHolder, funClause, problemDescription, range, null, new ChangeFunExpressionNameQuickFix(firstClauseName));
        }
      }
    }
  }

  @Nullable
  private static String getFunExpressionClauseName(ErlangFunClause clause) {
    ErlangArgumentDefinition argumentDefinition = clause.getArgumentDefinition();
    return argumentDefinition != null ? argumentDefinition.getText() : null;
  }

  private static class RenameFunctionClauseHeadQuickFix implements LocalQuickFix {
    private final String myFunctionName;

    protected RenameFunctionClauseHeadQuickFix(String functionName) {
      myFunctionName = functionName;
    }

    @Override
    public @IntentionName @NotNull String getName() {
      return "Rename clause head";
    }

    @Override
    public @IntentionFamilyName @NotNull String getFamilyName() {
      return getName();
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      PsiElement oldHead = descriptor.getPsiElement();
      if (oldHead instanceof ErlangQAtom) {
        ErlangPsiImplUtil.renameQAtom((ErlangQAtom) oldHead, myFunctionName);
      }
    }
  }

  private static class ChangeFunExpressionNameQuickFix implements LocalQuickFix {
    private final String myNewName;

    public ChangeFunExpressionNameQuickFix(@Nullable String newName) {
      myNewName = newName;
    }

    @Override
    public @IntentionName @NotNull String getName() {
      return "Change clause name";
    }

    @Override
    public @IntentionFamilyName @NotNull String getFamilyName() {
      return getName();
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      ErlangFunClause funClause = (ErlangFunClause) descriptor.getPsiElement();

      ErlangArgumentDefinition currentName = funClause.getArgumentDefinition();
      if (currentName != null) {
        currentName.delete();
      }

      if (myNewName != null) {
        ErlangArgumentDefinition newName = ErlangElementFactory.createFunExpressionNameFromText(project, myNewName);
        funClause.addBefore(newName, funClause.getFirstChild());
      }
    }
  }
}
