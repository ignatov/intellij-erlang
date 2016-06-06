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

public class ErlangGlobalFunctionCallExpressionImpl extends ErlangExpressionImpl implements ErlangGlobalFunctionCallExpression {

  public ErlangGlobalFunctionCallExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitGlobalFunctionCallExpression(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangFunctionCallExpression getFunctionCallExpression() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangFunctionCallExpression.class));
  }

  @Override
  @NotNull
  public ErlangModuleRef getModuleRef() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangModuleRef.class));
  }

  @Override
  @NotNull
  public PsiElement getColon() {
    return notNullChild(findChildByType(ERL_COLON));
  }

}
