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

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.quickfixes.ErlangRenameVariableFix;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangUnusedVariableInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    for (ErlangFunction function : ((ErlangFile) file).getFunctions()) {
      for (final ErlangFunctionClause functionClause : function.getFunctionClauseList()) {
        functionClause.accept(new ErlangRecursiveVisitor() {
          @Override
          public void visitQVar(@NotNull ErlangQVar o) {
            if (!isForceSkipped(o) && !isMacros(o) && ((inArgumentDefinition(o) && !inArgumentList(o)) || inLeftPartOfAssignment(o))) {
              PsiReference reference = o.getReference();
              PsiElement resolve = reference != null ? reference.resolve() : null;
              if (resolve == null) {
                Query<PsiReference> search = ReferencesSearch.search(o, new LocalSearchScope(functionClause));
                if (search.findFirst() == null) {
                  problemsHolder.registerProblem(o, "Unused variable " + "'" + o.getText() + "'", ProblemHighlightType.LIKE_UNUSED_SYMBOL, new ErlangRenameVariableFix());
                }
              }
            }
          }
        });
      }
    }
  }
}
