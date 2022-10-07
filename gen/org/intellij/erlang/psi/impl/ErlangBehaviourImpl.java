// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangBehaviourStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangBehaviourImpl extends ErlangStubbedPsiElementBase<ErlangBehaviourStub> implements ErlangBehaviour {

  public ErlangBehaviourImpl(@NotNull ErlangBehaviourStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangBehaviourImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitBehaviour(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangModuleRef getModuleRef() {
    return PsiTreeUtil.getChildOfType(this, ErlangModuleRef.class);
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
  public @NotNull String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

}
