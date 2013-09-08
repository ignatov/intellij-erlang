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

public class ErlangBeginEndExpressionImpl extends ErlangExpressionImpl implements ErlangBeginEndExpression {

  public ErlangBeginEndExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangBeginEndBody getBeginEndBody() {
    return findChildByClass(ErlangBeginEndBody.class);
  }

  @Override
  @NotNull
  public PsiElement getBegin() {
    return findNotNullChildByType(ERL_BEGIN);
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitBeginEndExpression(this);
    else super.accept(visitor);
  }

}
