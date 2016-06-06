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

public class ErlangFunTypeSigsBracesImpl extends ErlangCompositeElementImpl implements ErlangFunTypeSigsBraces {

  public ErlangFunTypeSigsBracesImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunTypeSigsBraces(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangFunTypeSigs getFunTypeSigs() {
    return PsiTreeUtil.getChildOfType(this, ErlangFunTypeSigs.class);
  }

  @Override
  @NotNull
  public PsiElement getParLeft() {
    return notNullChild(findChildByType(ERL_PAR_LEFT));
  }

  @Override
  @Nullable
  public PsiElement getParRight() {
    return findChildByType(ERL_PAR_RIGHT);
  }

}
