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

public class ErlangFunType100TImpl extends ErlangTypeImpl implements ErlangFunType100T {

  public ErlangFunType100TImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunType100T(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangTopTypeClause getTopTypeClause() {
    return PsiTreeUtil.getChildOfType(this, ErlangTopTypeClause.class);
  }

  @Override
  @NotNull
  public List<ErlangType> getTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangType.class);
  }

  @Override
  @NotNull
  public PsiElement getParLeft() {
    return notNullChild(findChildByType(ERL_PAR_LEFT));
  }

}
