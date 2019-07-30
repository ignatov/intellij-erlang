// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.psi.*;
import com.intellij.psi.PsiReference;

public class ErlangMacrosNameImpl extends ErlangCompositeElementImpl implements ErlangMacrosName {

  public ErlangMacrosNameImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitMacrosName(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangAtom getAtom() {
    return PsiTreeUtil.getChildOfType(this, ErlangAtom.class);
  }

  @Override
  @Nullable
  public PsiElement getVar() {
    return findChildByType(ERL_VAR);
  }

  @Override
  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @Override
  @Nullable
  public PsiReference getReference(@Nullable ErlangMacrosName o) {
    return ErlangPsiImplUtil.getReference(this, o);
  }

}
