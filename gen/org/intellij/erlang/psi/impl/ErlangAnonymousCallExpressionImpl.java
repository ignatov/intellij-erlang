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

public class ErlangAnonymousCallExpressionImpl extends ErlangExpressionImpl implements ErlangAnonymousCallExpression {

  public ErlangAnonymousCallExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitAnonymousCallExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangArgumentList getArgumentList() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangArgumentList.class));
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangExpression.class));
  }

}
