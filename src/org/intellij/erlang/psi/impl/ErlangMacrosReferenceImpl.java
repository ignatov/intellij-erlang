/*
 * Copyright 2012-2019 Sergey Ignatov
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

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementResolveResult;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangMacrosReferenceImpl extends PsiReferenceBase<PsiElement> implements PsiPolyVariantReference {
  private final ErlangMacrosName myNameElement;
  private final String myReferenceName;

  public ErlangMacrosReferenceImpl(@NotNull PsiElement owner, ErlangMacrosName element) {
    super(owner, ErlangPsiImplUtil.getTextRangeForReference(owner, element));
    myNameElement = element;
    myReferenceName = ErlangPsiImplUtil.getNameIdentifier(element).getText();
  }

  @Override
  public PsiElement handleElementRename(@NotNull String newElementName) throws IncorrectOperationException {
    ErlangPsiImplUtil.setName(myNameElement, newElementName);
    return myElement;
  }

  @Override
  public PsiElement resolve() {
    // Several possible definitions (conditional branches): no single target, as the platform
    // expects of a poly-variant reference, so that go to declaration offers them all; see ErlangMacroResolve.firstTarget
    ResolveResult[] r = multiResolve(false);
    return r.length == 1 ? r[0].getElement() : null;
  }

  /**
   * The definition with the use's arity, as epp chooses it, and every such definition in
   * the branches of a conditional, unless the file tells the active one (go to declaration then offers them all).
   */
  @Override
  public ResolveResult @NotNull [] multiResolve(boolean incompleteCode) {
    ErlangMacrosDefinition definition = PsiTreeUtil.getParentOfType(myNameElement, ErlangMacrosDefinition.class);
    if (definition != null && definition.getMacrosName() == myNameElement) return ResolveResult.EMPTY_ARRAY;
    if (!(myElement.getContainingFile() instanceof ErlangFile file)) return ResolveResult.EMPTY_ARRAY;
    // The name of -ifdef/-ifndef/-undef: what is defined at that point, any arity
    ErlangAttribute directive = ErlangMacroResolve.directiveOf(myNameElement);
    if (directive != null) return PsiElementResolveResult.createResults(ErlangMacroResolve.directiveTargets(file, directive, myReferenceName));
    int arity = ErlangMacroResolve.arityAfter(ErlangPsiImplUtil.getNameIdentifier(myNameElement));
    List<ErlangMacrosDefinition> all = ErlangMacroResolve.resolveAll(file, myReferenceName, arity, myNameElement);
    if (all.isEmpty()) {
      ErlangMacrosDefinition d = ErlangMacroResolve.resolve(file, myReferenceName, arity, myNameElement);
      return d == null ? ResolveResult.EMPTY_ARRAY : new ResolveResult[]{new PsiElementResolveResult(d)};
    }
    return PsiElementResolveResult.createResults(all);
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getMacrosLookupElements(myElement.getContainingFile()));
  }

  @Override
  public boolean isReferenceTo(@NotNull PsiElement element) {
    ErlangMacrosDefinition definition = ObjectUtils.tryCast(element, ErlangMacrosDefinition.class);
    String macroName = definition != null ? definition.getName() : null;
    if (macroName == null || !macroName.equals(myReferenceName) || definition.getMacrosName() == myNameElement) return false;
    // The name of another -define (another branch, another arity) is a definition, not a use
    if (myNameElement.getParent() instanceof ErlangMacrosDefinition d && d.getMacrosName() == myNameElement) return false;
    if (!(myElement.getContainingFile() instanceof ErlangFile file)) return true;
    // the name of a directive refers to what it acts on (every arity defined before it; see directiveTargets)
    ErlangAttribute directive = ErlangMacroResolve.directiveOf(myNameElement);
    if (directive != null) return ErlangMacroResolve.directiveTargets(file, directive, myReferenceName).contains(definition);
    // A definition that epp may choose for this use's arity (any conditional branch)
    List<ErlangMacrosDefinition> m = ErlangMacroResolve.matching(file, myReferenceName,
                                                                 ErlangMacroResolve.arityAfter(ErlangPsiImplUtil.getNameIdentifier(myNameElement)),
                                                                 myNameElement);
    if (!m.isEmpty()) return m.contains(definition);
    // Not a definition of the same name in an unrelated file (a header may use a macro only its includers define)
    return ErlangMacroResolve.isVisibleOrUnknown(file, definition);
  }
}
