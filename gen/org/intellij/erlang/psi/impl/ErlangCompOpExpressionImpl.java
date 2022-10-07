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

public class ErlangCompOpExpressionImpl extends ErlangFakeBinaryExpressionImpl implements ErlangCompOpExpression {

  public ErlangCompOpExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitCompOpExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getOpDivEq() {
    return findChildByType(ERL_OP_DIV_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpEqColEq() {
    return findChildByType(ERL_OP_EQ_COL_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpEqDivEq() {
    return findChildByType(ERL_OP_EQ_DIV_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpEqEq() {
    return findChildByType(ERL_OP_EQ_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpEqLt() {
    return findChildByType(ERL_OP_EQ_LT);
  }

  @Override
  @Nullable
  public PsiElement getOpGt() {
    return findChildByType(ERL_OP_GT);
  }

  @Override
  @Nullable
  public PsiElement getOpGtEq() {
    return findChildByType(ERL_OP_GT_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpLt() {
    return findChildByType(ERL_OP_LT);
  }

}
