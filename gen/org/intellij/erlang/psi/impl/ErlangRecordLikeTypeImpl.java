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

public class ErlangRecordLikeTypeImpl extends ErlangTypeImpl implements ErlangRecordLikeType {

  public ErlangRecordLikeTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitRecordLikeType(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangTopType> getTopTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTopType.class);
  }

  @Override
  @NotNull
  public PsiElement getCurlyLeft() {
    return findNotNullChildByType(ERL_CURLY_LEFT);
  }

  @Override
  @NotNull
  public PsiElement getCurlyRight() {
    return findNotNullChildByType(ERL_CURLY_RIGHT);
  }

}
