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

public class ErlangTypedAttrValImpl extends ErlangCompositeElementImpl implements ErlangTypedAttrVal {

  public ErlangTypedAttrValImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitTypedAttrVal(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangExpression.class));
  }

  @Override
  @Nullable
  public ErlangType getType() {
    return PsiTreeUtil.getChildOfType(this, ErlangType.class);
  }

  @Override
  @Nullable
  public ErlangTypedRecordFields getTypedRecordFields() {
    return PsiTreeUtil.getChildOfType(this, ErlangTypedRecordFields.class);
  }

  @Override
  @Nullable
  public PsiElement getColonColon() {
    return findChildByType(ERL_COLON_COLON);
  }

  @Override
  @Nullable
  public PsiElement getComma() {
    return findChildByType(ERL_COMMA);
  }

}
