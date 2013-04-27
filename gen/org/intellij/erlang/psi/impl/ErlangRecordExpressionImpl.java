// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRecordExpressionImpl extends ErlangExpressionImpl implements ErlangRecordExpression {

  public ErlangRecordExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return findNotNullChildByClass(ErlangExpression.class);
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return findChildByClass(ErlangMacros.class);
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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitRecordExpression(this);
    else super.accept(visitor);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

}
