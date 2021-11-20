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

public class ErlangRecordExpressionImpl extends ErlangExpressionImpl implements ErlangRecordExpression {

  public ErlangRecordExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitRecordExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangExpression.class));
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return PsiTreeUtil.getChildOfType(this, ErlangMacros.class);
  }

  @Override
  @Nullable
  public ErlangRecordField getRecordField() {
    return PsiTreeUtil.getChildOfType(this, ErlangRecordField.class);
  }

  @Override
  @Nullable
  public ErlangRecordRef getRecordRef() {
    return PsiTreeUtil.getChildOfType(this, ErlangRecordRef.class);
  }

  @Override
  @Nullable
  public ErlangRecordTuple getRecordTuple() {
    return PsiTreeUtil.getChildOfType(this, ErlangRecordTuple.class);
  }

  @Override
  @Nullable
  public PsiElement getRadix() {
    return findChildByType(ERL_RADIX);
  }

  @Override
  public @Nullable PsiReference getReferenceInternal() {
    return ErlangPsiImplUtil.getReferenceInternal(this);
  }

}
