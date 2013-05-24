// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangCaseExpression;
import org.intellij.erlang.psi.ErlangCrClause;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.intellij.erlang.ErlangTypes.*;

public class ErlangCaseExpressionImpl extends ErlangExpressionImpl implements ErlangCaseExpression {

  public ErlangCaseExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangCrClause> getCrClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangCrClause.class);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  @Override
  @NotNull
  public PsiElement getCase() {
    return findNotNullChildByType(ERL_CASE);
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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitCaseExpression(this);
    else super.accept(visitor);
  }

}
