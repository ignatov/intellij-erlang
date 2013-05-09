// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.ErlangAfterClause;
import org.intellij.erlang.psi.ErlangCrClauses;
import org.intellij.erlang.psi.ErlangReceiveExpression;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.intellij.erlang.ErlangTypes.ERL_END;
import static org.intellij.erlang.ErlangTypes.ERL_RECEIVE;

public class ErlangReceiveExpressionImpl extends ErlangExpressionImpl implements ErlangReceiveExpression {

  public ErlangReceiveExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangAfterClause getAfterClause() {
    return findChildByClass(ErlangAfterClause.class);
  }

  @Override
  @Nullable
  public ErlangCrClauses getCrClauses() {
    return findChildByClass(ErlangCrClauses.class);
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
