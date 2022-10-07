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

public class ErlangGenericFunctionCallExpressionImpl extends ErlangExpressionImpl implements ErlangGenericFunctionCallExpression {

  public ErlangGenericFunctionCallExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitGenericFunctionCallExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangArgumentList getArgumentList() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangArgumentList.class));
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return PsiTreeUtil.getChildOfType(this, ErlangMacros.class);
  }

  @Override
  @NotNull
  public List<ErlangQAtom> getQAtomList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQAtom.class);
  }

  @Override
  @NotNull
  public List<ErlangQVar> getQVarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQVar.class);
  }

  @Override
  @Nullable
  public PsiElement getColon() {
    return findChildByType(ERL_COLON);
  }

}
