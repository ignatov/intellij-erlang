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

public class ErlangTypedExprImpl extends ErlangNamedElementImpl implements ErlangTypedExpr {

  public ErlangTypedExprImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitTypedExpr(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return PsiTreeUtil.getChildOfType(this, ErlangExpression.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangQAtom.class));
  }

  @Override
  @Nullable
  public ErlangType getType() {
    return PsiTreeUtil.getChildOfType(this, ErlangType.class);
  }

  @Override
  @Nullable
  public PsiElement getColonColon() {
    return findChildByType(ERL_COLON_COLON);
  }

  @Override
  @Nullable
  public PsiElement getOpEq() {
    return findChildByType(ERL_OP_EQ);
  }

  @Override
  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @Override
  @NotNull
  public PsiElement setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @Override
  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

}
