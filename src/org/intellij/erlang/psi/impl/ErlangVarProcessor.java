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

package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangVarProcessor implements PsiScopeProcessor {
  public static final Key<Map<String, ErlangQVar>> ERLANG_VARIABLE_CONTEXT = Key.create("ERLANG_VARIABLE_CONTEXT");
  private final List<ErlangQVar> myVarList = new SmartList<>();
  private final String myRequestedName;
  private final PsiElement myOrigin;

  public ErlangVarProcessor(String requestedName, PsiElement origin) {
    myRequestedName = requestedName;
    myOrigin = origin;
  }

  @Override
  public boolean execute(@NotNull PsiElement psiElement, @NotNull ResolveState resolveState) {
    Map<String, ErlangQVar> variableContext = psiElement.getContainingFile().getOriginalFile().getUserData(ERLANG_VARIABLE_CONTEXT);
    if (variableContext != null) {
      ContainerUtil.addIfNotNull(myVarList, variableContext.get(myRequestedName));
      return true;
    }

    if (!(psiElement instanceof ErlangQVar)) return true;
    if (!psiElement.getText().equals(myRequestedName)) return true;
    if (psiElement.equals(myOrigin)) return true;
    if (inFunArgList(myOrigin)) {
      if (!inMapPatternKey(myOrigin)) return true;
      ErlangArgumentDefinitionList originArgList = PsiTreeUtil.getParentOfType(myOrigin, ErlangArgumentDefinitionList.class);
      if (PsiTreeUtil.isAncestor(originArgList, psiElement, false)) return true;
    }
    if (inFunArgList(psiElement) && inMapPatternKey(psiElement)) return true;
    ErlangFunExpression namedFun = getNamedFunOfName(myOrigin);
    if (namedFun != null && namedFun != getNamedFunOfName(psiElement)) return true;
    ErlangFunExpression candidateNamedFun = getNamedFunOfName(psiElement);
    if (candidateNamedFun != null && !PsiTreeUtil.isAncestor(candidateNamedFun, myOrigin, false)) return true;
    ErlangArgumentDefinition generatorPattern = getFreshGeneratorPattern(myOrigin);
    if (generatorPattern != null && !PsiTreeUtil.isAncestor(generatorPattern, psiElement, false)) return true;
    
    ErlangFunctionClause functionClause = PsiTreeUtil.getTopmostParentOfType(myOrigin, ErlangFunctionClause.class);
    ErlangSpecification spec = PsiTreeUtil.getTopmostParentOfType(myOrigin, ErlangSpecification.class);
    ErlangMacrosDefinition macroDefinition = PsiTreeUtil.getTopmostParentOfType(myOrigin, ErlangMacrosDefinition.class);

    boolean inSpecification = PsiTreeUtil.isAncestor(spec, psiElement, false);
    boolean inDefinition = inArgumentDefinition(psiElement);
    boolean inFunctionClause = PsiTreeUtil.isAncestor(functionClause, psiElement, false);
    boolean inAssignment = inLeftPartOfAssignment(psiElement);
    boolean inDefinitionOrAssignment = inDefinition || inAssignment;
    boolean inFunction = inFunctionClause && inDefinitionOrAssignment;
    boolean inMacroDefinition = PsiTreeUtil.isAncestor(macroDefinition, psiElement, false) && inDefinitionOrAssignment;
    boolean inBinaryWidthExpression = isBinaryWidthExpression(psiElement);
    
    if (inAssignment && inBinaryWidthExpression) return true;
    
    if (inFunction || inModule(psiElement) || inSpecification || inMacroDefinition) {
      boolean inArgumentList = inArgumentList(psiElement);
      boolean inArgumentListBeforeAssignment =
        PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentList.class, ErlangAssignmentExpression.class) instanceof ErlangArgumentList;
      if (inArgumentList && inArgumentListBeforeAssignment && !inDefinitionBeforeArgumentList(psiElement)) return true;
      if (inDifferentCrClauses(psiElement)) return true;
      if (hasNarrowerParentScope(psiElement)) return true;
      // put all possible variables to list
      myVarList.add((ErlangQVar) psiElement);
      return false;
    }

    return true;
  }

  private static boolean inFunArgList(PsiElement psiElement) {
    ErlangArgumentDefinitionList list = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinitionList.class);
    return list != null && list.getParent() instanceof ErlangFunClause;
  }

  // The name of a named fun shadows an outer variable with the same name.
  private static ErlangFunExpression getNamedFunOfName(PsiElement psiElement) {
    ErlangArgumentDefinition definition = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class);
    PsiElement clause = definition != null ? definition.getParent() : null;
    if (!(clause instanceof ErlangFunClause) || ((ErlangFunClause) clause).getArgumentDefinition() != definition) return null;
    return PsiTreeUtil.getParentOfType(clause, ErlangFunExpression.class);
  }

  // A generator pattern binds fresh variables that shadow outer ones, except in map keys and binary sizes.
  private static ErlangArgumentDefinition getFreshGeneratorPattern(PsiElement psiElement) {
    ErlangArgumentDefinition definition = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class);
    if (definition == null || !(definition.getParent() instanceof ErlangLcExpression)) return null;
    if (inMapPatternKey(psiElement) || isBinaryWidthExpression(psiElement)) return null;
    return definition;
  }

  private static boolean inMapPatternKey(PsiElement psiElement) {
    ErlangMapEntry entry = PsiTreeUtil.getParentOfType(psiElement, ErlangMapEntry.class, true, ErlangArgumentDefinitionList.class);
    while (entry != null) {
      List<ErlangExpression> expressions = entry.getExpressionList();
      if (entry.getMatch() != null && !expressions.isEmpty() && PsiTreeUtil.isAncestor(expressions.get(0), psiElement, false)) return true;
      entry = PsiTreeUtil.getParentOfType(entry, ErlangMapEntry.class, true, ErlangArgumentDefinitionList.class);
    }
    return false;
  }

  private boolean hasNarrowerParentScope(PsiElement psiElement) {
    ErlangCompositeElement narrowestParentScopeOwner = PsiTreeUtil.getParentOfType(psiElement,
                                                                                   ErlangCrClause.class,
                                                                                   ErlangFunClause.class,
                                                                                   ErlangFunctionClause.class,
                                                                                   ErlangListComprehension.class);
    if (narrowestParentScopeOwner instanceof ErlangFunClause) {
      ErlangArgumentDefinition funName = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class);
      if (((ErlangFunClause) narrowestParentScopeOwner).getArgumentDefinition() == funName) {
        // the name of a named fun is declared by its first clause only and is visible only inside that fun
        if (funName == null) return false;
        if (PsiTreeUtil.getPrevSiblingOfType(narrowestParentScopeOwner, ErlangFunClause.class) != null) return true;
        ErlangFunExpression funExpression = PsiTreeUtil.getParentOfType(narrowestParentScopeOwner, ErlangFunExpression.class);
        return !PsiTreeUtil.isAncestor(funExpression, myOrigin, false);
      }
    }
    if (narrowestParentScopeOwner instanceof ErlangCrClause && isExportedTo(narrowestParentScopeOwner, myOrigin)) return false;
    return narrowestParentScopeOwner != null && !PsiTreeUtil.isAncestor(narrowestParentScopeOwner, myOrigin, false);
  }

  // A case or receive exports the variables of its clauses to the clause body around it, and further out
  // through every case or receive clause around that body, but not out of a fun or a comprehension.
  // Inside a comprehension the qualifier order is checked by the resolve walk.
  private static boolean isExportedTo(@NotNull ErlangCompositeElement clause, @NotNull PsiElement origin) {
    PsiElement expression = clause.getParent();
    while (expression instanceof ErlangCaseExpression || expression instanceof ErlangReceiveExpression) {
      PsiElement body = PsiTreeUtil.getParentOfType(expression, ErlangClauseBody.class, ErlangListComprehension.class, ErlangFunClause.class);
      boolean visibleInBody = !PsiTreeUtil.isAncestor(expression, origin, false) && PsiTreeUtil.isAncestor(body, origin, false);
      if (!(body instanceof ErlangClauseBody)) return visibleInBody && body instanceof ErlangListComprehension;
      if (visibleInBody) return true;
      PsiElement bodyOwner = body.getParent();
      expression = bodyOwner instanceof ErlangCrClause || bodyOwner instanceof ErlangAfterClauseBody ? bodyOwner.getParent() : null;
    }
    return false;
  }

  private boolean inDifferentCrClauses(PsiElement psiElement) {
    ErlangCrClause crClause = PsiTreeUtil.getParentOfType(psiElement, ErlangCrClause.class);
    ErlangCrClause crClauseOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCrClause.class);

    if (crClause == null || crClauseOrigin == null) return false;
    if (crClause.getParent() != crClauseOrigin.getParent()) return false;

    ErlangCompositeElement caseExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangCaseExpression.class, ErlangReceiveExpression.class);
    ErlangCompositeElement caseExpressionOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCaseExpression.class, ErlangReceiveExpression.class);

    if (caseExpressionOrigin != null && caseExpression == caseExpressionOrigin && crClause != crClauseOrigin) return true;
    return false;
  }

  @NotNull
  public List<ErlangQVar> getAllResults() {
    return myVarList;
  }
}
