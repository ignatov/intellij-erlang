// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangSpecFun;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.intellij.erlang.ErlangTypes.ERL_INTEGER;

public class ErlangSpecFunImpl extends ErlangCompositeElementImpl implements ErlangSpecFun {

  public ErlangSpecFunImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return findNotNullChildByClass(ErlangQAtom.class);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitSpecFun(this);
    else super.accept(visitor);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

}
