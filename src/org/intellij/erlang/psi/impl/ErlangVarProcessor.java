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

package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

/**
 * @author ignatov
 */
public class ErlangVarProcessor extends BaseScopeProcessor {
  public static final Key<Map<String, ErlangQVar>> ERLANG_VARIABLE_CONTEXT = Key.create("ERLANG_VARIABLE_CONTEXT");
  private List<ErlangQVar> myVarList = new ArrayList<ErlangQVar>(0);
  private final String myRequestedName;
  private final PsiElement myOrigin;

  public ErlangVarProcessor(String requestedName, PsiElement origin) {
    myRequestedName = requestedName;
    myOrigin = origin;
  }

  @Override
  public boolean execute(@NotNull PsiElement psiElement, ResolveState resolveState) {
    if (psiElement instanceof ErlangFunction) return false;
    if (psiElement instanceof ErlangSpecification) return false;
    ErlangFunctionClause functionClause = PsiTreeUtil.getTopmostParentOfType(myOrigin, ErlangFunctionClause.class);
    ErlangSpecification spec = PsiTreeUtil.getTopmostParentOfType(myOrigin, ErlangSpecification.class);

    Map<String, ErlangQVar> variableContext = psiElement.getContainingFile().getOriginalFile().getUserData(ERLANG_VARIABLE_CONTEXT);
    boolean isREPLConsole = variableContext != null;
    if (!psiElement.equals(myOrigin) && psiElement instanceof ErlangQVar && psiElement.getText().equals(myRequestedName)) {
      boolean inSpecification = PsiTreeUtil.isAncestor(spec, psiElement, false);
      boolean inDefinition = inArgumentDefinition(psiElement);
      boolean inFunctionClauseOrREPL = PsiTreeUtil.isAncestor(functionClause, psiElement, false) || isREPLConsole;
      boolean inAssignment = inLeftPartOfAssignment(psiElement);
      boolean inDefinitionOrAssignment = inDefinition || inAssignment;
      boolean inFunctionOrREPL = inFunctionClauseOrREPL && inDefinitionOrAssignment;
      if (inFunctionOrREPL || inModule(psiElement) || inSpecification) {
        boolean inArgumentList = inArgumentList(psiElement);
        //noinspection unchecked
        boolean inArgumentListBeforeAssignment =
          PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentList.class, ErlangAssignmentExpression.class) instanceof ErlangArgumentList;
        if (inArgumentList && inArgumentListBeforeAssignment && !inDefinitionBeforeArgumentList(psiElement)) return true;
        if (inDifferentCrClauses(psiElement)) return true;
        myVarList.add((ErlangQVar) psiElement); // put all possible variables to list
      }
    }

    if (isREPLConsole) {
      ContainerUtil.addIfNotNull(variableContext.get(myRequestedName), myVarList);
    }

    return true;
  }

  private boolean inDifferentCrClauses(PsiElement psiElement) {
    ErlangCrClause crClause = PsiTreeUtil.getParentOfType(psiElement, ErlangCrClause.class);
    ErlangCrClause crClauseOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCrClause.class);

    ErlangCaseExpression caseExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangCaseExpression.class);
    ErlangCaseExpression caseExpressionOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCaseExpression.class);

    if (caseExpressionOrigin != null && caseExpression == caseExpressionOrigin && crClause != crClauseOrigin) return true;
    return false;
  }

  @Nullable
  public ErlangQVar getResult() {
    return ContainerUtil.getFirstItem(myVarList); // return the topmost one
  }
}
