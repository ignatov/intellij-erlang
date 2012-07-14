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

public class ErlangLcExpressionImpl extends ErlangExpressionImpl implements ErlangLcExpression {

  public ErlangLcExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangArgumentDefinition getArgumentDefinition() {
    return findNotNullChildByClass(ErlangArgumentDefinition.class);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitLcExpression(this);
    else super.accept(visitor);
  }

}
