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

import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.FileContentUtilCore;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;

//TODO add arity mismatch checks
public class ErlangHeadMismatchInspection extends ErlangInspectionBase implements DumbAware {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunction(@NotNull ErlangFunction function) {
        super.visitFunction(function); // for fun expressions
        checkFunction(function, problemsHolder);
      }

      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression funExpression) {
        super.visitFunExpression(funExpression); // for nested fun expressions
        checkFunExpression(funExpression, problemsHolder);
      }
    });
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
        problemsHolder.registerProblem(clauseHead,
          "Head mismatch: should be '" + functionSignature + "'", new RenameFunctionClauseHeadQuickFix(functionName));
      }
    }
  }

  private static void checkFunExpression(ErlangFunExpression funExpression, ProblemsHolder problemsHolder) {
    ErlangFunClauses funClauses = funExpression.getFunClauses();
    List<ErlangFunClause> funClauseList = funClauses != null ? funClauses.getFunClauseList() : Collections.<ErlangFunClause>emptyList();
    if (funClauseList.size() <= 1) return;
    ErlangFunClause firstClause = funClauseList.get(0);
    ErlangQVar firstClauseName = getFunExpressionClauseName(firstClause);
    for (ErlangFunClause funClause : funClauseList) {
      ErlangQVar funClauseName = getFunExpressionClauseName(funClause);
      if (!funExpressionClauseNamesAreEqual(firstClauseName, funClauseName)) {
        String problemDescription = firstClauseName == null ?
          "Head mismatch: named clause in an unnamed fun expression" :
          (funClauseName == null ?
            "Head mismatch: unnamed clause in a named fun expression" :
            "Head mismatch: should be '" + firstClauseName.getName() + "'");
        PsiElement elementForRange = funClauseName != null ? funClauseName : funClause.getArgumentDefinitionList();
        TextRange range = TextRange.create(elementForRange.getStartOffsetInParent(),
          elementForRange.getStartOffsetInParent() + elementForRange.getTextLength());
        problemsHolder.registerProblem(funClause, range, problemDescription, new ChangeFunExpressionNameQuickFix(firstClauseName));
      }
    }
  }

  private static boolean funExpressionClauseNamesAreEqual(@Nullable ErlangQVar var1, @Nullable ErlangQVar var2) {
    return var1 == var2 || var1 != null && var2 != null && var1.getName().equals(var2.getName());
  }

  @Nullable
  private static ErlangQVar getFunExpressionClauseName(ErlangFunClause clause) {
    ErlangArgumentDefinition argumentDefinition = clause.getArgumentDefinition();
    ErlangExpression expression = argumentDefinition != null ? argumentDefinition.getExpression() : null;
    return expression instanceof ErlangMaxExpression ? ((ErlangMaxExpression) expression).getQVar() : null;
  }

  private static class RenameFunctionClauseHeadQuickFix extends LocalQuickFixBase {
    private final String myFunctionName;

    protected RenameFunctionClauseHeadQuickFix(String functionName) {
      super("Rename clause head");
      myFunctionName = functionName;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      PsiElement oldHead = descriptor.getPsiElement();
      if (oldHead instanceof ErlangQAtom) {
        ErlangPsiImplUtil.renameAtom((ErlangQAtom) oldHead, myFunctionName);
      }
    }
  }

  private static class ChangeFunExpressionNameQuickFix extends LocalQuickFixBase {
    private PsiElement myNewNameVar;

    public ChangeFunExpressionNameQuickFix(@Nullable ErlangQVar newNameVar) {
      super("Change clause name");
      myNewNameVar = newNameVar != null ? newNameVar.getVar() : null;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      ErlangFunClause funClause = (ErlangFunClause) descriptor.getPsiElement();
      ErlangQVar actualNameQVar = getFunExpressionClauseName(funClause);
      PsiElement actualNameVar = actualNameQVar != null ? actualNameQVar.getVar() : null;
      if (myNewNameVar == null) {
        if (actualNameVar != null) {
          actualNameVar.delete();
          rebuildPsiFor(funClause);
        }
      }
      else {
        if (actualNameVar == null) {
          funClause.addBefore(myNewNameVar, funClause.getFirstChild());
        }
        else {
          actualNameVar.replace(myNewNameVar);
        }
        rebuildPsiFor(funClause);
      }
    }

    private static void rebuildPsiFor(PsiElement what) {
      PsiFile psiFile = what.getContainingFile();
      VirtualFile virtualFile = psiFile.getVirtualFile();
      if (virtualFile != null) {
        FileContentUtilCore.reparseFiles(Collections.singletonList(virtualFile));
      }
    }
  }
}
