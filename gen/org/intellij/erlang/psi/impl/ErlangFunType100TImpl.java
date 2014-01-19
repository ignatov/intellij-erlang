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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunType100T(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangTopType> getTopTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTopType.class);
  }

  @Override
  @Nullable
  public ErlangTopTypeClause getTopTypeClause() {
    return findChildByClass(ErlangTopTypeClause.class);
  }

  @Override
  @Nullable
  public PsiElement getDotDotDot() {
    return findChildByType(ERL_DOT_DOT_DOT);
  }

  @Override
  @NotNull
  public PsiElement getParLeft() {
    return findNotNullChildByType(ERL_PAR_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getParRight() {
    return findChildByType(ERL_PAR_RIGHT);
  }

}
