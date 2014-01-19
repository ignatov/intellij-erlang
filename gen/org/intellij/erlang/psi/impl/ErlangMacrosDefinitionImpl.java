// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangMacrosDefinitionStub;
import org.intellij.erlang.psi.*;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangMacrosDefinitionImpl extends ErlangStubbedPsiElementBase<ErlangMacrosDefinitionStub> implements ErlangMacrosDefinition {

  public ErlangMacrosDefinitionImpl(ASTNode node) {
    super(node);
  }

  public ErlangMacrosDefinitionImpl(ErlangMacrosDefinitionStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitMacrosDefinition(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinitionList getArgumentDefinitionList() {
    return findChildByClass(ErlangArgumentDefinitionList.class);
  }

  @Override
  @Nullable
  public ErlangMacrosBody getMacrosBody() {
    return findChildByClass(ErlangMacrosBody.class);
  }

  @Override
  @Nullable
  public ErlangMacrosName getMacrosName() {
    return findChildByClass(ErlangMacrosName.class);
  }

  @Override
  @Nullable
  public PsiElement getComma() {
    return findChildByType(ERL_COMMA);
  }

  @Override
  @NotNull
  public PsiElement getOpMinus() {
    return findNotNullChildByType(ERL_OP_MINUS);
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

  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @NotNull
  public PsiElement setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

}
