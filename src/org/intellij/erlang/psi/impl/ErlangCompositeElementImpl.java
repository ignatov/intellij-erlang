package org.intellij.erlang.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangCompositeElementImpl extends ASTWrapperPsiElement implements ErlangCompositeElement {
  public ErlangCompositeElementImpl(ASTNode node) {
    super(node);
  }

  @Override
  public String toString() {
    return getNode().getElementType().toString();
  }

  @Override
  public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    if (!processor.execute(this, state)) {
      return false;
    } else {
      return ResolveUtil.processChildren(this, processor, state, lastParent, place);
//      for (PsiElement element : getChildren()) {
//        if (!element.processDeclarations(processor, state, lastParent, place)) return false;
//      }
//      return true;
    }
//    return ResolveUtil.processChildren(this, processor, state, lastParent, place);
  }
}
