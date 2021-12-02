// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangRecordDefinitionStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangRecordDefinitionImpl extends ErlangNamedStubbedPsiElementBase<ErlangRecordDefinitionStub> implements ErlangRecordDefinition {

  public ErlangRecordDefinitionImpl(@NotNull ErlangRecordDefinitionStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangRecordDefinitionImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitRecordDefinition(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangQAtom getQAtom() {
    return PsiTreeUtil.getChildOfType(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public ErlangTypedRecordFields getTypedRecordFields() {
    return PsiTreeUtil.getChildOfType(this, ErlangTypedRecordFields.class);
  }

  @Override
  @Nullable
  public PsiElement getComma() {
    return findChildByType(ERL_COMMA);
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

}
