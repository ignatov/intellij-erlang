// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.intellij.erlang.ErlangTypes.ERL_OF;
import static org.intellij.erlang.ErlangTypes.ERL_TRY;

public class ErlangTryExpressionImpl extends ErlangExpressionImpl implements ErlangTryExpression {

  public ErlangTryExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangCrClause> getCrClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangCrClause.class);
  }

  @Override
  @Nullable
  public ErlangTryCatch getTryCatch() {
    return findChildByClass(ErlangTryCatch.class);
  }

  @Override
  @Nullable
  public ErlangTryExpressionsClause getTryExpressionsClause() {
    return findChildByClass(ErlangTryExpressionsClause.class);
  }

  @Override
  @Nullable
  public PsiElement getOf() {
    return findChildByType(ERL_OF);
  }

  @Override
  @NotNull
  public PsiElement getTry() {
    return findNotNullChildByType(ERL_TRY);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTryExpression(this);
    else super.accept(visitor);
  }

}
