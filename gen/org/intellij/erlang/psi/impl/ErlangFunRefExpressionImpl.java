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

public class ErlangFunRefExpressionImpl extends ErlangExpressionImpl implements ErlangFunRefExpression {

  public ErlangFunRefExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunRefExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangFunctionWithArity getFunctionWithArity() {
    return PsiTreeUtil.getChildOfType(this, ErlangFunctionWithArity.class);
  }

  @Override
  @Nullable
  public ErlangFunctionWithArityVariables getFunctionWithArityVariables() {
    return PsiTreeUtil.getChildOfType(this, ErlangFunctionWithArityVariables.class);
  }

  @Override
  @Nullable
  public ErlangModuleRef getModuleRef() {
    return PsiTreeUtil.getChildOfType(this, ErlangModuleRef.class);
  }

  @Override
  @Nullable
  public ErlangQVar getQVar() {
    return PsiTreeUtil.getChildOfType(this, ErlangQVar.class);
  }

  @Override
  @Nullable
  public PsiElement getColon() {
    return findChildByType(ERL_COLON);
  }

  @Override
  @NotNull
  public PsiElement getFun() {
    return notNullChild(findChildByType(ERL_FUN));
  }

}
