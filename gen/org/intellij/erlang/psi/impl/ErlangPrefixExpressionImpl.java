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

public class ErlangPrefixExpressionImpl extends ErlangExpressionImpl implements ErlangPrefixExpression {

  public ErlangPrefixExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return findNotNullChildByClass(ErlangExpression.class);
  }

  @Override
  @Nullable
  public PsiElement getBnot() {
    return findChildByType(ERL_BNOT);
  }

  @Override
  @Nullable
  public PsiElement getNot() {
    return findChildByType(ERL_NOT);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitPrefixExpression(this);
    else super.accept(visitor);
  }

}
