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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangHeadMismatchInspection extends ErlangInspectionBase implements DumbAware {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunction(@NotNull ErlangFunction function) {
        checkFunction(function, problemsHolder);
      }
    });
  }

  private static void checkFunction(ErlangFunction function, ProblemsHolder problemsHolder) {
    final String functionName = function.getName();
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
        PsiElement newHead = ErlangElementFactory.createQAtomFromText(project, myFunctionName);
        PsiElement atom = ((ErlangQAtom) oldHead).getAtom();
        if (atom != null) {
          atom.replace(newHead);
        }
      }
    }
  }
}
