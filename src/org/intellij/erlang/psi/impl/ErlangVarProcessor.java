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

import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangVarProcessor implements PsiScopeProcessor {
  public static final Key<Map<String, ErlangQVar>> ERLANG_VARIABLE_CONTEXT = Key.create("ERLANG_VARIABLE_CONTEXT");
  private final List<ErlangQVar> myVarList = new SmartList<>();
  private final String myRequestedName;
  private final PsiElement myOrigin;
  // the earlier bindings of the variables met in this resolve; null when the processor collects only the variables it meets
  @Nullable private final Map<ErlangQVar, List<ErlangQVar>> myEarlierBindings;

  public ErlangVarProcessor(String requestedName, PsiElement origin) {
    this(requestedName, origin, new HashMap<>());
  }

  private ErlangVarProcessor(String requestedName, PsiElement origin, @Nullable Map<ErlangQVar, List<ErlangQVar>> earlierBindings) {
    myRequestedName = requestedName;
    myOrigin = origin;
    myEarlierBindings = earlierBindings;
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
    if (isNamedFunDeclaration(myOrigin)) return true;
    if (inFunArgList(myOrigin)) {
      ErlangArgumentDefinitionList originArgList = PsiTreeUtil.getParentOfType(myOrigin, ErlangArgumentDefinitionList.class);
      boolean inOriginArgList = PsiTreeUtil.isAncestor(originArgList, psiElement, false);
      // a map key in a fun head refers to an outer variable; any other variable there shadows the outer ones
      // and matches only an earlier occurrence in the same head
      if (inMapPatternKey(myOrigin)) {
        if (inOriginArgList) return true;
      }
      else if (!inOriginArgList || psiElement.getTextOffset() > myOrigin.getTextOffset()) return true;
    }
    if (inFunArgList(psiElement) && inMapPatternKey(psiElement)) return true;
    
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
      if (boundInOwnOrLaterQualifier(psiElement)) return true;
      if (outsideOwnGeneratorPattern(psiElement)) return true;
      if (boundAfterOrigin(psiElement)) return true;
      // put all possible variables to list
      ErlangQVar var = (ErlangQVar) psiElement;
      List<ErlangQVar> found = myEarlierBindings == null || var.getTextOffset() >= myOrigin.getTextOffset()
                               ? List.of(var) : getEarlierBindings(var, myEarlierBindings);
      for (ErlangQVar v : found) {
        if (!myVarList.contains(v)) myVarList.add(v);
      }
      return false;
    }

    return true;
  }

  // a variable that matches an earlier binding, like X in `X = 1, case X = g() of ...`, is not a binding itself.
  // Only earlier variables are followed, so the lookup always moves towards the start of the file and terminates.
  // The lookup is iterative and remembers the answer for every variable, so each variable is resolved once per resolve.
  @NotNull
  private static List<ErlangQVar> getEarlierBindings(@NotNull ErlangQVar var, @NotNull Map<ErlangQVar, List<ErlangQVar>> earlierBindings) {
    Map<ErlangQVar, List<ErlangQVar>> candidates = new HashMap<>();
    Deque<ErlangQVar> stack = new ArrayDeque<>();
    stack.push(var);
    while (!stack.isEmpty()) {
      ProgressManager.checkCanceled();
      ErlangQVar top = stack.peek();
      if (earlierBindings.containsKey(top)) {
        stack.pop();
        continue;
      }
      List<ErlangQVar> earlier = candidates.computeIfAbsent(top, ErlangVarProcessor::getEarlierCandidates);
      boolean ready = true;
      for (ErlangQVar candidate : earlier) {
        if (!earlierBindings.containsKey(candidate)) {
          stack.push(candidate);
          ready = false;
        }
      }
      if (!ready) continue;
      Set<ErlangQVar> bindings = new LinkedHashSet<>();
      for (ErlangQVar candidate : earlier) {
        bindings.addAll(earlierBindings.get(candidate));
      }
      earlierBindings.put(top, bindings.isEmpty() ? List.of(top) : List.copyOf(bindings));
      stack.pop();
    }
    return earlierBindings.get(var);
  }

  // the variables before var that a resolve of var meets, without following them further
  @NotNull
  private static List<ErlangQVar> getEarlierCandidates(@NotNull ErlangQVar var) {
    int offset = var.getTextOffset();
    List<ErlangQVar> result = new SmartList<>();
    for (ErlangQVar candidate : ErlangVariableReferenceImpl.processVariables(var, new ErlangVarProcessor(var.getText(), var, null))) {
      if (candidate.getTextOffset() < offset && !result.contains(candidate)) result.add(candidate);
    }
    return result;
  }

  // the name in the first clause of a named fun declares a new variable that shadows any outer one
  private static boolean isNamedFunDeclaration(@NotNull PsiElement psiElement) {
    ErlangArgumentDefinition definition = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class);
    return definition != null
           && definition.getParent() instanceof ErlangFunClause clause
           && clause.getArgumentDefinition() == definition
           && PsiTreeUtil.getPrevSiblingOfType(clause, ErlangFunClause.class) == null;
  }

  private static boolean inFunArgList(PsiElement psiElement) {
    ErlangArgumentDefinitionList list = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinitionList.class);
    return list != null && list.getParent() instanceof ErlangFunClause;
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
    if (narrowestParentScopeOwner instanceof ErlangCrClause) {
      ErlangCaseExpression caseExpr = PsiTreeUtil.getParentOfType(psiElement, ErlangCaseExpression.class);
      if (Comparing.equal(
        PsiTreeUtil.getParentOfType(caseExpr, ErlangClauseBody.class),
        PsiTreeUtil.getParentOfType(myOrigin, ErlangClauseBody.class))) return false;
    }
    return narrowestParentScopeOwner != null && !PsiTreeUtil.isAncestor(narrowestParentScopeOwner, myOrigin, false);
  }

  // a qualifier of a comprehension sees neither the pattern of its own generator nor the later qualifiers
  private boolean boundInOwnOrLaterQualifier(PsiElement psiElement) {
    for (ErlangLcExpression qualifier = PsiTreeUtil.getParentOfType(myOrigin, ErlangLcExpression.class);
         qualifier != null;
         qualifier = PsiTreeUtil.getParentOfType(qualifier, ErlangLcExpression.class)) {
      ErlangArgumentDefinition pattern = qualifier.getArgumentDefinition();
      if (pattern != null && !PsiTreeUtil.isAncestor(pattern, myOrigin, false) && PsiTreeUtil.isAncestor(pattern, psiElement, false)) return true;
      for (PsiElement next = qualifier.getNextSibling(); next != null; next = next.getNextSibling()) {
        if (next instanceof ErlangLcExpression && PsiTreeUtil.isAncestor(next, psiElement, false)) return true;
      }
    }
    return false;
  }

  // the left side of a match is bound after its right side is evaluated, and a clause body after the head and the guard of its clause
  private boolean boundAfterOrigin(PsiElement psiElement) {
    for (PsiElement parent = psiElement.getParent(); parent != null && !(parent instanceof PsiFile); parent = parent.getParent()) {
      if (parent instanceof ErlangFakeBinaryExpression match && (parent instanceof ErlangAssignmentExpression || parent instanceof ErlangMaybeMatchExpression)) {
        if (PsiTreeUtil.isAncestor(match.getLeft(), psiElement, false) && PsiTreeUtil.isAncestor(match.getRight(), myOrigin, false) && !inPattern(match)) return true;
      }
      else if (parent instanceof ErlangClauseBody body) {
        if (!PsiTreeUtil.isAncestor(body, myOrigin, false) && PsiTreeUtil.isAncestor(body.getParent(), myOrigin, false)) return true;
      }
    }
    return false;
  }

  // in a pattern, like the head of a clause or the left side of a match, `=` is an alias and both of its sides are bound together
  private static boolean inPattern(@NotNull PsiElement match) {
    return PsiTreeUtil.getParentOfType(match, ErlangArgumentDefinition.class, true, ErlangClauseBody.class) != null
           || inLeftPartOfAssignment(match);
  }

  // a variable bound in the pattern of a generator is a new one, which shadows any variable of the same name outside the pattern;
  // a map key of the pattern is a use of a variable bound before the pattern, never a binding
  private boolean outsideOwnGeneratorPattern(PsiElement psiElement) {
    if (inMapPatternKey(psiElement) && inGeneratorPattern(psiElement)) return true;
    ErlangArgumentDefinition pattern = PsiTreeUtil.getParentOfType(myOrigin, ErlangArgumentDefinition.class);
    if (pattern == null || !(pattern.getParent() instanceof ErlangLcExpression)) return false;
    if (isBinaryWidthExpression(myOrigin)) return false;
    boolean inPattern = PsiTreeUtil.isAncestor(pattern, psiElement, false);
    return inMapPatternKey(myOrigin) == inPattern;
  }

  private static boolean inGeneratorPattern(PsiElement psiElement) {
    ErlangArgumentDefinition pattern = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class);
    return pattern != null && pattern.getParent() instanceof ErlangLcExpression;
  }

  private boolean inDifferentCrClauses(PsiElement psiElement) {
    ErlangCrClause crClause = PsiTreeUtil.getParentOfType(psiElement, ErlangCrClause.class);
    ErlangCrClause crClauseOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCrClause.class);

    if (crClause == null || crClauseOrigin == null) return false;
    if (crClause.getParent() != crClauseOrigin.getParent()) return false;

    ErlangCaseExpression caseExpression = PsiTreeUtil.getParentOfType(psiElement, ErlangCaseExpression.class);
    ErlangCaseExpression caseExpressionOrigin = PsiTreeUtil.getParentOfType(myOrigin, ErlangCaseExpression.class);

    if (caseExpressionOrigin != null && caseExpression == caseExpressionOrigin && crClause != crClauseOrigin) return true;
    return false;
  }

  @NotNull
  public List<ErlangQVar> getAllResults() {
    return myVarList;
  }
}
