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

public class ErlangBinUnitTypeImpl extends ErlangTypeImpl implements ErlangBinUnitType {

  public ErlangBinUnitTypeImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitBinUnitType(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangQVar> getQVarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQVar.class);
  }

  @Override
  @NotNull
  public PsiElement getColon() {
    return notNullChild(findChildByType(ERL_COLON));
  }

  @Override
  @NotNull
  public PsiElement getOpArMul() {
    return notNullChild(findChildByType(ERL_OP_AR_MUL));
  }

  @Override
  @NotNull
  public PsiElement getInteger() {
    return notNullChild(findChildByType(ERL_INTEGER));
  }

}
