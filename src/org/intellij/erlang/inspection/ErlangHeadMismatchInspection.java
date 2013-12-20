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
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangHeadMismatchInspection extends ErlangInspectionBase implements DumbAware {
  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    for (ErlangFunction function : ((ErlangFile) file).getFunctions()) {
      final String functionName = function.getName();
      List<ErlangFunctionClause> clauses = function.getFunctionClauseList();

      if (clauses.size() > 1) {
        for (ErlangFunctionClause clause : clauses) {
          ErlangQAtom clauseHead = clause.getQAtom();
          if (clauseHead.getMacros() != null) return;
          String clauseSignature = ErlangPsiImplUtil.createFunctionClausePresentation(clause);
          String functionSignature = ErlangPsiImplUtil.createFunctionPresentation(function);

          if (!functionSignature.equals(clauseSignature)) {
            problemsHolder.registerProblem(clauseHead, "Head mismatch: should be '" + functionSignature + "'",
              new LocalQuickFixBase("Rename clause head") {
                @Override
                public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
                  PsiElement oldHead = descriptor.getPsiElement();
                  if (oldHead instanceof ErlangQAtom) {
                    PsiElement newHead = ErlangElementFactory.createQAtomFromText(project, functionName);
                    PsiElement atom = ((ErlangQAtom) oldHead).getAtom();
                    if (atom != null) {
                      atom.replace(newHead);
                    }
                  }
                }
              });
          }
        }
      }
    }
  }
}
