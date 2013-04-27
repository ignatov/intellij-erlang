// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangTypedExprImpl extends ErlangNamedElementImpl implements ErlangTypedExpr {

  public ErlangTypedExprImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return findChildByClass(ErlangMacros.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return findNotNullChildByClass(ErlangQAtom.class);
  }

  @Override
  @Nullable
  public ErlangRecordField getRecordField() {
    return findChildByClass(ErlangRecordField.class);
  }

  @Override
  @Nullable
  public ErlangRecordRef getRecordRef() {
    return findChildByClass(ErlangRecordRef.class);
  }

  @Override
  @Nullable
  public ErlangRecordTuple getRecordTuple() {
    return findChildByClass(ErlangRecordTuple.class);
  }

  @Override
  @Nullable
  public ErlangTopType getTopType() {
    return findChildByClass(ErlangTopType.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTypedExpr(this);
    else super.accept(visitor);
  }

  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @NotNull
  public PsiElement setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

}
