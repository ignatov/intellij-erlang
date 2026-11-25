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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFunClause;
import org.intellij.erlang.psi.ErlangFunClauses;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.statistics.ErlangUsageCounters;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangRenameVariableFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Rename variable (will start with _)";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangUsageCounters.countQuickFix(project, getClass());
    PsiElement element = descriptor.getPsiElement();

    if (element instanceof ErlangQVar var) {
      String newName = "_" + var.getName();
      for (ErlangQVar v : getNamedFunClauseNames(var)) {
        v.setName(newName);
      }
    }
  }

  // the name of a named fun is repeated in every clause of that fun, so all of them are renamed together
  @NotNull
  private static List<ErlangQVar> getNamedFunClauseNames(@NotNull ErlangQVar var) {
    List<ErlangQVar> result = new ArrayList<>();
    result.add(var);
    if (!ErlangPsiImplUtil.isNamedFunClauseName(var)) return result;
    ErlangFunClauses clauses = PsiTreeUtil.getParentOfType(var, ErlangFunClauses.class);
    if (clauses == null) return result;
    for (ErlangFunClause clause : clauses.getFunClauseList()) {
      if (clause.getArgumentDefinition() == null) continue;
      ErlangQVar name = PsiTreeUtil.findChildOfType(clause.getArgumentDefinition(), ErlangQVar.class);
      if (name != null && name != var && var.getText().equals(name.getText())) result.add(name);
    }
    return result;
  }
}
