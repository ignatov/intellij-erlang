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

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.text.CaseInsensitiveStringHashingStrategy;
import gnu.trove.THashSet;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangVariableReferenceImpl extends PsiPolyVariantReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @NotNull
  @Override
  public ResolveResult[] multiResolve(boolean b) {
    ErlangVarProcessor processor = new ErlangVarProcessor(myElement.getText(), myElement);
    ErlangListComprehension lc = PsiTreeUtil.getParentOfType(myElement, ErlangListComprehension.class);
    ErlangCompositeElement place = lc != null ? lc.getLcExprs() : myElement;
    ResolveUtil.treeWalkUp(place, processor);
    List<ErlangQVar> result = processor.getAllResults();
    if (!result.isEmpty()) return PsiElementResolveResult.createResults(result);

    ErlangModule module = getErlangModule();
    if (module == null) return ResolveResult.EMPTY_ARRAY;
    module.processDeclarations(processor, ResolveState.initial(), module, module);

    List<ErlangQVar> allResults = processor.getAllResults();
    return PsiElementResolveResult.createResults(allResults);
  }

  @Nullable
  @Override
  public PsiElement resolve() {
    ResolveResult[] resolveResults = multiResolve(false);
    return resolveResults.length > 0 ? resolveResults[0].getElement() : null;
  }

  @Override
  public boolean isReferenceTo(PsiElement element) {
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
  public Object[] getVariants() {
    if (PsiTreeUtil.getParentOfType(myElement, ErlangArgumentDefinition.class) != null) return new Object[]{};

    List<LookupElement> result = new ArrayList<LookupElement>();
    Collection<String> vars = new THashSet<String>(CaseInsensitiveStringHashingStrategy.INSTANCE);
    PsiFile file = myElement.getContainingFile();
    if (!(myElement.getParent() instanceof ErlangRecordExpression)) {
      //noinspection unchecked
      PsiElement scopeOwner = PsiTreeUtil.getParentOfType(myElement,
        ErlangFunctionClause.class, ErlangMacrosDefinition.class, ErlangTypeDefinition.class, ErlangSpecification.class);
      ResolveUtil.treeWalkUp(myElement, new MyBaseScopeProcessor(scopeOwner, false, vars));

      ErlangModule module = getErlangModule();
      if (module != null) {
        module.processDeclarations(new MyBaseScopeProcessor(scopeOwner, true, vars), ResolveState.initial(), module, module);
      }

      for (String var : vars) {
        result.add(LookupElementBuilder.create(var).withIcon(ErlangIcons.VARIABLE));
      }

      ErlangQAtom qAtom = getQAtom(PsiTreeUtil.getParentOfType(myElement, ErlangColonQualifiedExpression.class));
      boolean withArity = myElement.getParent() instanceof ErlangFunctionWithArityVariables;
      result.addAll(ErlangPsiImplUtil.getFunctionLookupElements(file, withArity, false, qAtom));
    }

    Map<String, ErlangQVar> context = file.getOriginalFile().getUserData(ErlangVarProcessor.ERLANG_VARIABLE_CONTEXT);
    if (context != null && PsiTreeUtil.getParentOfType(myElement, ErlangColonQualifiedExpression.class) == null) {
      for (String var : context.keySet()) {
        result.add(LookupElementBuilder.create(var).withIcon(ErlangIcons.VARIABLE));
      }
    }

    return ArrayUtil.toObjectArray(result);
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    myElement.replace(ErlangElementFactory.createQVarFromText(myElement.getProject(), newElementName));
    return myElement;
  }

  private class MyBaseScopeProcessor extends BaseScopeProcessor {
    private final PsiElement myScopeOwner;
    private final boolean myForce;
    private final Collection<String> myResult;
    @Nullable private Collection<ErlangQVar> myVars;

    public MyBaseScopeProcessor(@Nullable PsiElement scopeOwner, boolean force, Collection<String> result) {
      myScopeOwner = scopeOwner;
      myForce = force;
      myResult = result;
    }

    @SuppressWarnings("NullableProblems")
    private MyBaseScopeProcessor(@Nullable PsiElement scopeOwner, @NotNull Collection<ErlangQVar> result) {
      this(scopeOwner, true, ContainerUtil.<String>newArrayList());
      myVars = result;
    }

    @Override
    public boolean execute(@NotNull PsiElement psiElement, @NotNull ResolveState resolveState) {
      if (!psiElement.equals(myElement) && psiElement instanceof ErlangQVar && !psiElement.getText().equals("_") && !inColonQualified(myElement)) {
        boolean ancestor = PsiTreeUtil.isAncestor(myScopeOwner, psiElement, false);
        if ((ancestor || myForce) && (inArgumentDefinition(psiElement) || inLeftPartOfAssignment(psiElement) || inFunctionTypeArgument(psiElement))) {
          myResult.add(((ErlangQVar) psiElement).getName());
          if (myVars != null) myVars.add((ErlangQVar) psiElement);
        }
      }
      return true;
    }
  }

  public void populateVariables(@Nullable PsiElement scopeOwner, @NotNull Collection<ErlangQVar> result) {
    ResolveUtil.treeWalkUp(myElement, new MyBaseScopeProcessor(scopeOwner, result));
  }
}
