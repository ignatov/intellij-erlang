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
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitBeginEndExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangBeginEndBody getBeginEndBody() {
    return PsiTreeUtil.getChildOfType(this, ErlangBeginEndBody.class);
  }

  @Override
  @NotNull
  public PsiElement getBegin() {
    return notNullChild(findChildByType(ERL_BEGIN));
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

}
