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

public class ErlangTryExpressionImpl extends ErlangExpressionImpl implements ErlangTryExpression {

  public ErlangTryExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTryExpression(this);
    else super.accept(visitor);
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
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
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

}
