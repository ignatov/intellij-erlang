// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangModuleStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangModuleImpl extends ErlangNamedStubbedPsiElementBase<ErlangModuleStub> implements ErlangModule {

  public ErlangModuleImpl(@NotNull ErlangModuleStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangModuleImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitModule(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinition getArgumentDefinition() {
    return PsiTreeUtil.getChildOfType(this, ErlangArgumentDefinition.class);
  }

  @Override
  @Nullable
  public ErlangModelFieldList getModelFieldList() {
    return PsiTreeUtil.getChildOfType(this, ErlangModelFieldList.class);
  }

  @Override
  @Nullable
  public ErlangQAtom getQAtom() {
    return PsiTreeUtil.getChildOfType(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public PsiElement getComma() {
    return findChildByType(ERL_COMMA);
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

  @Override
  public @NotNull PsiElement setName(@NotNull String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @Override
  public @NotNull PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

  @Override
  public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return ErlangPsiImplUtil.processDeclarations(this, processor, state, lastParent, place);
  }

}
