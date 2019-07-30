// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangIncludeStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangIncludeImpl extends ErlangStubbedPsiElementBase<ErlangIncludeStub> implements ErlangInclude {

  public ErlangIncludeImpl(ASTNode node) {
    super(node);
  }

  public ErlangIncludeImpl(ErlangIncludeStub stub, IStubElementType stubType) {
    super(stub, stubType);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitInclude(this);
  }

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
  @Nullable
  public ErlangIncludeString getIncludeStringSafe() {
    return ErlangPsiImplUtil.getIncludeStringSafe(this);
  }

}
