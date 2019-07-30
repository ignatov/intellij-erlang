// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangCallbackSpecStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangCallbackSpecImpl extends ErlangStubbedPsiElementBase<ErlangCallbackSpecStub> implements ErlangCallbackSpec {

  public ErlangCallbackSpecImpl(ASTNode node) {
    super(node);
  }

  public ErlangCallbackSpecImpl(ErlangCallbackSpecStub stub, IStubElementType stubType) {
    super(stub, stubType);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitCallbackSpec(this);
  }

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
  public boolean isOptional() {
    return ErlangPsiImplUtil.isOptional(this);
  }

}
