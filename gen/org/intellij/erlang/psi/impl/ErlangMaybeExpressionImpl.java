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

public class ErlangMaybeExpressionImpl extends ErlangExpressionImpl implements ErlangMaybeExpression {

  public ErlangMaybeExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitMaybeExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangCrClause> getCrClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangCrClause.class);
  }

  @Override
  @Nullable
  public ErlangMaybeMatchExprs getMaybeMatchExprs() {
    return PsiTreeUtil.getChildOfType(this, ErlangMaybeMatchExprs.class);
  }

  @Override
  @Nullable
  public PsiElement getElse() {
    return findChildByType(ERL_ELSE);
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

  @Override
  @NotNull
  public PsiElement getMaybe() {
    return notNullChild(findChildByType(ERL_MAYBE));
  }

}
