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

public class ErlangFunctionWithArityImpl extends ErlangCompositeElementImpl implements ErlangFunctionWithArity {

  public ErlangFunctionWithArityImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunctionWithArity(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangQAtom.class));
  }

  @Override
  @Nullable
  public PsiElement getOpArDiv() {
    return findChildByType(ERL_OP_AR_DIV);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  @Override
  public @Nullable PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @Override
  public @Nullable PsiReference getReference(@Nullable ErlangMacrosName o) {
    return ErlangPsiImplUtil.getReference(this, o);
  }

}
