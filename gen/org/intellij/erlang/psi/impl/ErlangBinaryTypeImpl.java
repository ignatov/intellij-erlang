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

public class ErlangBinaryTypeImpl extends ErlangTypeImpl implements ErlangBinaryType {

  public ErlangBinaryTypeImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitBinaryType(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangType> getTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangType.class);
  }

  @Override
  @Nullable
  public PsiElement getBinEnd() {
    return findChildByType(ERL_BIN_END);
  }

  @Override
  @NotNull
  public PsiElement getBinStart() {
    return notNullChild(findChildByType(ERL_BIN_START));
  }

  @Override
  @Nullable
  public PsiElement getComma() {
    return findChildByType(ERL_COMMA);
  }

}
