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
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.fromTheSameCaseExpression;

public class ErlangVariableReferenceImpl extends ErlangPsiPolyVariantCachingReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @NotNull
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
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
    List<ErlangQVar> result = new SmartList<>();
    for (ErlangQVar candidate : resolveInScope(var)) {
      boolean earlierInSameFile = candidate.getContainingFile() == var.getContainingFile() &&
                                  candidate.getTextOffset() < var.getTextOffset();
      boolean follow = earlierInSameFile && !inGeneratorPattern(candidate);
      List<ErlangQVar> bindings = follow ? resolveToFirstBindings(candidate) : Collections.emptyList();
      for (ErlangQVar binding : bindings.isEmpty() ? Collections.singletonList(candidate) : bindings) {
        if (!result.contains(binding)) result.add(binding);
      }
    }
    return result;
  }

  // A generator pattern binds a fresh variable that shadows an outer one with the same name.
  private static boolean inGeneratorPattern(@NotNull ErlangQVar var) {
    ErlangArgumentDefinition definition = PsiTreeUtil.getParentOfType(var, ErlangArgumentDefinition.class);
    return definition != null && definition.getParent() instanceof ErlangLcExpression;
  }

  @NotNull
  private static List<ErlangQVar> resolveInScope(@NotNull ErlangQVar var) {
    ErlangVarProcessor processor = new ErlangVarProcessor(var.getText(), var);
    ErlangLcExpression lc = PsiTreeUtil.getParentOfType(var, ErlangLcExpression.class);
    ErlangCompositeElement place = ObjectUtils.chooseNotNull(lc, var);
    ResolveUtil.treeWalkUp(place, processor);
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
