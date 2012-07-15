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

public class ErlangReceiveExpressionImpl extends ErlangExpressionImpl implements ErlangReceiveExpression {

  public ErlangReceiveExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangClauseBody getClauseBody() {
    return findChildByClass(ErlangClauseBody.class);
  }

  @Override
  @Nullable
  public ErlangCrClauses getCrClauses() {
    return findChildByClass(ErlangCrClauses.class);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  @Override
  @Nullable
  public PsiElement getAfter() {
    return findChildByType(ERL_AFTER);
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

  @Override
  @NotNull
  public PsiElement getReceive() {
    return findNotNullChildByType(ERL_RECEIVE);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitReceiveExpression(this);
    else super.accept(visitor);
  }

}
