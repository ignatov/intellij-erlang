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

public class ErlangConfigExpressionImpl extends ErlangExpressionImpl implements ErlangConfigExpression {

  public ErlangConfigExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitConfigExpression(this);
  }

  @Override
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
  public List<ErlangMacros> getMacrosList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangMacros.class);
  }

  @Override
  @Nullable
  public ErlangQAtom getQAtom() {
    return PsiTreeUtil.getChildOfType(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public ErlangQVar getQVar() {
    return PsiTreeUtil.getChildOfType(this, ErlangQVar.class);
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
  public PsiElement getChar() {
    return findChildByType(ERL_CHAR);
  }

  @Override
  @Nullable
  public PsiElement getFloat() {
    return findChildByType(ERL_FLOAT);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  @Override
  @Nullable
  public PsiElement getNot() {
    return findChildByType(ERL_NOT);
  }

}
