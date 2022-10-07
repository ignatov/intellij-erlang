// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangCallbackFunctionStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangCallbackFunctionImpl extends ErlangStubbedPsiElementBase<ErlangCallbackFunctionStub> implements ErlangCallbackFunction {

  public ErlangCallbackFunctionImpl(@NotNull ErlangCallbackFunctionStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangCallbackFunctionImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitCallbackFunction(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangQAtom.class));
  }

  @Override
  @Nullable
  public PsiElement getOpArDiv() {
    return findChildByType(ERL_OP_AR_DIV);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  @Override
  public @NotNull PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @Override
  public @Nullable PsiReference getReference(@Nullable ErlangMacrosName o) {
    return ErlangPsiImplUtil.getReference(this, o);
  }

}
