package org.intellij.erlang.psi.impl;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.jetbrains.annotations.NotNull;

public class ErlangStubbedPsiElementBase<T extends StubElement<?>> extends StubBasedPsiElementBase<T> implements ErlangCompositeElement {
  public ErlangStubbedPsiElementBase(@NotNull T stub, @NotNull IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public ErlangStubbedPsiElementBase(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return ErlangCompositeElementImpl.processDeclarations(this, processor, state, lastParent, place);
  }

  @Override
  public String toString() {
    return getElementType().toString();
  }
}
