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

public class ErlangIntTypeImpl extends ErlangTypeImpl implements ErlangIntType {

  public ErlangIntTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitIntType(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangArgumentList getArgumentList() {
    return PsiTreeUtil.getChildOfType(this, ErlangArgumentList.class);
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return PsiTreeUtil.getChildOfType(this, ErlangMacros.class);
  }

  @Override
  @Nullable
  public PsiElement getOpMinus() {
    return findChildByType(ERL_OP_MINUS);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

}
