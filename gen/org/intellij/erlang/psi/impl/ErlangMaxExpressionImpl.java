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

public class ErlangMaxExpressionImpl extends ErlangExpressionImpl implements ErlangMaxExpression {

  public ErlangMaxExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitMaxExpression(this);
  }

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

}
