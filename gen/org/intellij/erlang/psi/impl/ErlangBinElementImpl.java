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

public class ErlangBinElementImpl extends ErlangCompositeElementImpl implements ErlangBinElement {

  public ErlangBinElementImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitBinElement(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangExpression> getExpressionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangExpression.class);
  }

  @Override
  @Nullable
  public ErlangOptBitTypeList getOptBitTypeList() {
    return PsiTreeUtil.getChildOfType(this, ErlangOptBitTypeList.class);
  }

  @Override
  @Nullable
  public PsiElement getColon() {
    return findChildByType(ERL_COLON);
  }

  @Override
  @Nullable
  public PsiElement getOpMinus() {
    return findChildByType(ERL_OP_MINUS);
  }

  @Override
  @Nullable
  public PsiElement getOpPlus() {
    return findChildByType(ERL_OP_PLUS);
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

}
