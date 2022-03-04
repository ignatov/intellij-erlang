// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangIncludeLibStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangIncludeLibImpl extends ErlangStubbedPsiElementBase<ErlangIncludeLibStub> implements ErlangIncludeLib {

  public ErlangIncludeLibImpl(@NotNull ErlangIncludeLibStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangIncludeLibImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitIncludeLib(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangIncludeString getIncludeString() {
    return PsiTreeUtil.getChildOfType(this, ErlangIncludeString.class);
  }

  @Override
  @NotNull
  public PsiElement getOpMinus() {
    return notNullChild(findChildByType(ERL_OP_MINUS));
  }

  @Override
  @Nullable
  public PsiElement getParLeft() {
    return findChildByType(ERL_PAR_LEFT);
  }

  @Override
  @Nullable
  public PsiElement getParRight() {
    return findChildByType(ERL_PAR_RIGHT);
  }

  @Override
  public @Nullable ErlangIncludeString getIncludeStringSafe() {
    return ErlangPsiImplUtil.getIncludeStringSafe(this);
  }

}
