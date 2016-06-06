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

public class ErlangLcExpressionImpl extends ErlangExpressionImpl implements ErlangLcExpression {

  public ErlangLcExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitLcExpression(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinition getArgumentDefinition() {
    return PsiTreeUtil.getChildOfType(this, ErlangArgumentDefinition.class);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangExpression.class));
  }

  @Override
  @Nullable
  public PsiElement getOpLtEq() {
    return findChildByType(ERL_OP_LT_EQ);
  }

  @Override
  @Nullable
  public PsiElement getOpLtMinus() {
    return findChildByType(ERL_OP_LT_MINUS);
  }

}
