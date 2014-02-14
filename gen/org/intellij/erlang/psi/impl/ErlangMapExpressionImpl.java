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

public class ErlangMapExpressionImpl extends ErlangExpressionImpl implements ErlangMapExpression {

  public ErlangMapExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitMapExpression(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return findNotNullChildByClass(ErlangExpression.class);
  }

  @Override
  @Nullable
  public ErlangMapEntries getMapEntries() {
    return findChildByClass(ErlangMapEntries.class);
  }

  @Override
  @NotNull
  public PsiElement getCurlyLeft() {
    return findNotNullChildByType(ERL_CURLY_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getCurlyRight() {
    return findChildByType(ERL_CURLY_RIGHT);
  }

  @Override
  @NotNull
  public PsiElement getRadix() {
    return findNotNullChildByType(ERL_RADIX);
  }

}
