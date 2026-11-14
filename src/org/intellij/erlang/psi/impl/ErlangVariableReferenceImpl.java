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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.fromTheSameCaseExpression;

public class ErlangVariableReferenceImpl extends ErlangPsiPolyVariantCachingReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @NotNull
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    Set<ErlangQVar> result = new LinkedHashSet<>();
    for (ErlangQVar candidate : resolveNearest(myElement)) {
      result.add(followToBinding(candidate));
    }
    return PsiElementResolveResult.createResults(result);
  }

  /**
   * The nearest candidate can itself be a match of an already bound variable,
   * e.g. the head of {@code case X = f() of _ -> X end} after {@code X = 1}.
   * Follow such matches back to the binding so that every occurrence of a variable resolves to the same element.
   */
  @NotNull
  private ErlangQVar followToBinding(@NotNull ErlangQVar candidate) {
    Set<ErlangQVar> visited = new HashSet<>();
    visited.add(myElement);
    ErlangQVar current = candidate;
    while (visited.add(current)) {
      List<ErlangQVar> next = resolveNearest(current);
      if (next.size() != 1) return current;
      current = next.get(0);
    }
    return candidate;
  }

  @NotNull
  private static List<ErlangQVar> resolveNearest(@NotNull ErlangQVar var) {
    ErlangVarProcessor processor = new ErlangVarProcessor(var.getText(), var);
    ErlangLcExpression lc = PsiTreeUtil.getParentOfType(var, ErlangLcExpression.class);
    ErlangCompositeElement place = ObjectUtils.chooseNotNull(lc, var);
    ResolveUtil.treeWalkUp(place, processor);
    List<ErlangQVar> result = processor.getAllResults();
    if (!result.isEmpty()) return result;

    ErlangModule module = getErlangModule(var);
    if (module == null) return result;
    module.processDeclarations(processor, ResolveState.initial(), module, module);
    return processor.getAllResults();
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
  private static ErlangModule getErlangModule(@NotNull PsiElement element) {
    PsiFile file = element.getContainingFile();
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
