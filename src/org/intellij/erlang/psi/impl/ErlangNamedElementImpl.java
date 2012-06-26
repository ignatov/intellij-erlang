package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public abstract class ErlangNamedElementImpl extends ErlangCompositeElementImpl implements ErlangCompositeElement, ErlangNamedElement {
  public ErlangNamedElementImpl(ASTNode node) {
    super(node);
  }

  @Override
  public PsiElement setName(@NonNls @NotNull String s) throws IncorrectOperationException {
    return null;
  }
}
