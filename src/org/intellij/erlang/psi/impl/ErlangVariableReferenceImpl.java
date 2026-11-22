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

import java.util.List;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.fromTheSameCaseExpression;

public class ErlangVariableReferenceImpl extends ErlangPsiPolyVariantCachingReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @NotNull
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    List<ErlangQVar> result = processVariables(myElement, new ErlangVarProcessor(myElement.getText(), myElement));
    return result.isEmpty() ? ResolveResult.EMPTY_ARRAY : PsiElementResolveResult.createResults(result);
  }

  @NotNull
  static List<ErlangQVar> processVariables(@NotNull ErlangQVar element, @NotNull ErlangVarProcessor processor) {
    ErlangLcExpression lc = PsiTreeUtil.getParentOfType(element, ErlangLcExpression.class);
    ErlangCompositeElement place = ObjectUtils.chooseNotNull(lc, element);
    ResolveUtil.treeWalkUp(place, processor);
    List<ErlangQVar> result = processor.getAllResults();
    if (!result.isEmpty()) return result;

    ErlangModule module = getErlangModule(element);
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
