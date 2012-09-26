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

public class ErlangCaseExpressionImpl extends ErlangExpressionImpl implements ErlangCaseExpression {

  public ErlangCaseExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangCrClauses getCrClauses() {
    return findNotNullChildByClass(ErlangCrClauses.class);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return findNotNullChildByClass(ErlangExpression.class);
  }

  @Override
  @NotNull
  public PsiElement getCase() {
    return findNotNullChildByType(ERL_CASE);
  }

  @Override
  @NotNull
  public PsiElement getEnd() {
    return findNotNullChildByType(ERL_END);
  }

  @Override
  @NotNull
  public PsiElement getOf() {
    return findNotNullChildByType(ERL_OF);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitCaseExpression(this);
    else super.accept(visitor);
  }

}
