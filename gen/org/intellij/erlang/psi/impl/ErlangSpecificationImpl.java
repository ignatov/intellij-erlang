// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangSpecificationStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangSpecificationImpl extends ErlangNamedStubbedPsiElementBase<ErlangSpecificationStub> implements ErlangSpecification {

  public ErlangSpecificationImpl(@NotNull ErlangSpecificationStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangSpecificationImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitSpecification(this);
  }

  @Override
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
  @Nullable
  public ErlangFunTypeSigsBraces getFunTypeSigsBraces() {
    return PsiTreeUtil.getChildOfType(this, ErlangFunTypeSigsBraces.class);
  }

  @Override
  public @Nullable ErlangFunTypeSigs getSignature() {
    return ErlangPsiImplUtil.getSignature(this);
  }

  @Override
  public @NotNull PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public @NotNull String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @Override
  public @NotNull PsiElement setName(@NotNull String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @Override
  public int getArity() {
    return ErlangPsiImplUtil.getArity(this);
  }

}
