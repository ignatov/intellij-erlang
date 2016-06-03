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

  public ErlangSpecificationImpl(ASTNode node) {
    super(node);
  }

  public ErlangSpecificationImpl(ErlangSpecificationStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitSpecification(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangFunTypeSigs getFunTypeSigs() {
    return findChildByClass(ErlangFunTypeSigs.class);
  }

  @Override
  @Nullable
  public ErlangFunTypeSigsBraces getFunTypeSigsBraces() {
    return findChildByClass(ErlangFunTypeSigsBraces.class);
  }

  @Nullable
  public ErlangFunTypeSigs getSignature() {
    return ErlangPsiImplUtil.getSignature(this);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @NotNull
  public PsiElement setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  public int getArity() {
    return ErlangPsiImplUtil.getArity(this);
  }

}
