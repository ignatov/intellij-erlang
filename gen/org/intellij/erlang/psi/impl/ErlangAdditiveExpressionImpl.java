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

public class ErlangAdditiveExpressionImpl extends ErlangFakeBinaryExpressionImpl implements ErlangAdditiveExpression {

  public ErlangAdditiveExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitAdditiveExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getOpMinus() {
    return findChildByType(ERL_OP_MINUS);
  }

  @Override
  @Nullable
  public PsiElement getOpPlus() {
    return findChildByType(ERL_OP_PLUS);
  }

  @Override
  @Nullable
  public PsiElement getBor() {
    return findChildByType(ERL_BOR);
  }

  @Override
  @Nullable
  public PsiElement getBsl() {
    return findChildByType(ERL_BSL);
  }

  @Override
  @Nullable
  public PsiElement getBsr() {
    return findChildByType(ERL_BSR);
  }

  @Override
  @Nullable
  public PsiElement getBxor() {
    return findChildByType(ERL_BXOR);
  }

  @Override
  @Nullable
  public PsiElement getOr() {
    return findChildByType(ERL_OR);
  }

  @Override
  @Nullable
  public PsiElement getXor() {
    return findChildByType(ERL_XOR);
  }

}
