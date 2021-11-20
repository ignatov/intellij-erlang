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

public class ErlangListOpExpressionImpl extends ErlangFakeBinaryExpressionImpl implements ErlangListOpExpression {

  public ErlangListOpExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitListOpExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getOpMinusMinus() {
    return findChildByType(ERL_OP_MINUS_MINUS);
  }

  @Override
  @Nullable
  public PsiElement getOpPlusPlus() {
    return findChildByType(ERL_OP_PLUS_PLUS);
  }

}
