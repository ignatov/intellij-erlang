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
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.resolve.ResolveCache;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.fromTheSameCaseExpression;

public class ErlangVariableReferenceImpl extends ErlangPsiPolyVariantCachingReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  private static final ResolveCache.PolyVariantResolver<ErlangVariableReferenceImpl> MY_POLY_RESOLVER =
    (reference, incompleteCode) -> reference.multiResolveInner();

  @NotNull
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    return ResolveCache.getInstance(myElement.getProject()).resolveWithCaching(this, MY_POLY_RESOLVER, false, incompleteCode);
  }

  private ResolveResult @NotNull [] multiResolveInner() {
    List<ErlangQVar> result = resolveToFirstBindings(myElement);
    if (!result.isEmpty()) return PsiElementResolveResult.createResults(result);

    ErlangModule module = getErlangModule();
    if (module == null) return ResolveResult.EMPTY_ARRAY;
    ErlangVarProcessor processor = new ErlangVarProcessor(myElement.getText(), myElement);
    module.processDeclarations(processor, ResolveState.initial(), module, module);

    List<ErlangQVar> allResults = processor.getAllResults();
    return PsiElementResolveResult.createResults(allResults);
  }

  // The nearest earlier occurrence in a pattern can itself be a use of an earlier binding
  // (`Pid = self(), receive {x, Pid} -> Pid end`), so follow it back to the first binding.
  @NotNull
  private static List<ErlangQVar> resolveToFirstBindings(@NotNull ErlangQVar var) {
    List<ErlangQVar> candidates = resolveInScope(var);
    resolveUncachedEarlierCandidates(var, candidates);
    List<ErlangQVar> result = new SmartList<>();
    for (ErlangQVar candidate : candidates) {
      List<ErlangQVar> bindings = isEarlier(candidate, var) ? resolveCandidate(candidate) : Collections.emptyList();
      for (ErlangQVar binding : bindings.isEmpty() ? Collections.singletonList(candidate) : bindings) {
        if (!result.contains(binding)) result.add(binding);
      }
    }
    return result;
  }

  // Resolves the not yet cached part of the chain oldest first, so that every resolve finds
  // its candidates in the cache: a long chain neither recurses deeply nor is walked twice.
  private static void resolveUncachedEarlierCandidates(@NotNull ErlangQVar var, @NotNull List<ErlangQVar> candidates) {
    Set<ErlangQVar> uncached = new HashSet<>();
    Deque<ErlangQVar> queue = new ArrayDeque<>();
    for (ErlangQVar candidate : candidates) {
      if (isEarlier(candidate, var)) queue.add(candidate);
    }
    while (!queue.isEmpty()) {
      ProgressManager.checkCanceled();
      ErlangQVar candidate = queue.remove();
      if (uncached.contains(candidate) || isResolveCached(candidate)) continue;
      uncached.add(candidate);
      for (ErlangQVar earlier : resolveInScope(candidate)) {
        if (isEarlier(earlier, candidate)) queue.add(earlier);
      }
    }
    if (uncached.size() < 2) return;
    List<ErlangQVar> oldestFirst = new ArrayList<>(uncached);
    oldestFirst.sort((a, b) -> a == b ? 0 : isEarlier(a, b) ? -1 : 1);
    for (ErlangQVar candidate : oldestFirst) {
      resolveCandidate(candidate);
    }
  }

  private static boolean isResolveCached(@NotNull ErlangQVar var) {
    PsiReference reference = var.getReference();
    return !(reference instanceof ErlangVariableReferenceImpl) ||
           ResolveCache.getInstance(var.getProject())
             .getCachedResults((ErlangVariableReferenceImpl) reference, var.getContainingFile().isPhysical(), false, true) != null;
  }

  // Evaluation order: the head of a comprehension comes after its generators and filters.
  private static boolean isEarlier(@NotNull ErlangQVar candidate, @NotNull ErlangQVar var) {
    if (candidate == var || candidate.getContainingFile() != var.getContainingFile()) return false;
    PsiElement commonParent = PsiTreeUtil.findCommonParent(candidate, var);
    ErlangListComprehension comprehension = PsiTreeUtil.getParentOfType(commonParent, ErlangListComprehension.class, false);
    if (comprehension != null) {
      boolean candidateInHead = inHead(candidate, comprehension);
      boolean varInHead = inHead(var, comprehension);
      if (candidateInHead != varInHead) return varInHead;
    }
    return candidate.getTextOffset() < var.getTextOffset();
  }

  private static boolean inHead(@NotNull PsiElement element, @NotNull ErlangListComprehension comprehension) {
    PsiElement orOr = comprehension.getOrOr();
    return orOr != null && element.getTextOffset() < orOr.getTextOffset();
  }

  @NotNull
  private static List<ErlangQVar> resolveCandidate(@NotNull ErlangQVar candidate) {
    List<ErlangQVar> result = new SmartList<>();
    PsiReference reference = candidate.getReference();
    if (!(reference instanceof ErlangVariableReferenceImpl)) return result;
    for (ResolveResult resolveResult : ((ErlangVariableReferenceImpl) reference).multiResolve(false)) {
      if (resolveResult.getElement() instanceof ErlangQVar binding) result.add(binding);
    }
    return result;
  }

  // Walks up like ResolveUtil.treeWalkUp, but sees in a comprehension only the qualifiers evaluated before
  // the variable, and never the pattern of the generator that the variable is in.
  @NotNull
  private static List<ErlangQVar> resolveInScope(@NotNull ErlangQVar var) {
    ErlangVarProcessor processor = new ErlangVarProcessor(var.getText(), var);
    PsiElement lastParent = var;
    for (PsiElement run = var.getParent(); run != null; lastParent = run, run = run.getParent()) {
      if (run instanceof ErlangLcExpression) continue;
      boolean proceed = run instanceof ErlangListComprehension comprehension && comprehension.getOrOr() != null
                        ? processEarlierQualifiers(comprehension, lastParent, processor, var)
                        : run.processDeclarations(processor, ResolveState.initial(), lastParent, var);
      if (!proceed) break;
    }
    return processor.getAllResults();
  }

  private static boolean processEarlierQualifiers(@NotNull ErlangListComprehension comprehension,
                                                  @NotNull PsiElement lastParent,
                                                  @NotNull ErlangVarProcessor processor,
                                                  @NotNull ErlangQVar place) {
    PsiElement orOr = comprehension.getOrOr();
    PsiElement run = inHead(lastParent, comprehension) ? comprehension.getLastChild() : lastParent.getPrevSibling();
    for (; run != null && run != orOr; run = run.getPrevSibling()) {
      // only the pattern of a generator binds variables; its source does not export them
      PsiElement qualifier = run instanceof ErlangLcExpression lc ? lc.getArgumentDefinition() : run;
      if (qualifier instanceof ErlangCompositeElement &&
          !qualifier.processDeclarations(processor, ResolveState.initial(), null, place)) {
        return false;
      }
    }
    return true;
  }

  @Nullable
  @Override
  public PsiElement resolveInner() {
    ResolveResult[] resolveResults = multiResolve(false);
    return resolveResults.length > 0 ? resolveResults[0].getElement() : null;
  }

  @Override
  public boolean isReferenceTo(@NotNull PsiElement element) {
    if (!(element instanceof ErlangQVar)) return false;
    for (ResolveResult result : multiResolve(false)) {
      PsiElement resultElement = result.getElement();
      if (resultElement instanceof ErlangQVar && resultElement.isEquivalentTo(element)) return true;
    }
    if (fromTheSameCaseExpression(getElement(), element)) return true;
    return false;
  }

  @Nullable
  private ErlangModule getErlangModule() {
    PsiFile file = myElement.getContainingFile();
    ErlangFile erlangFile = file instanceof ErlangFile ? (ErlangFile) file : null;
    return erlangFile != null ? erlangFile.getModule() : null;
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    return ArrayUtil.EMPTY_OBJECT_ARRAY;
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    myElement.replace(ErlangElementFactory.createQVarFromText(myElement.getProject(), newElementName));
    return myElement;
  }
}
