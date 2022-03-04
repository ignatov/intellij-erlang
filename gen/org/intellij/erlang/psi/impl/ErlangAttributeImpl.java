// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.psi.*;

public class ErlangAttributeImpl extends ErlangCompositeElementImpl implements ErlangAttribute {

  public ErlangAttributeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitAttribute(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangAtomAttribute getAtomAttribute() {
    return PsiTreeUtil.getChildOfType(this, ErlangAtomAttribute.class);
  }

  @Override
  @Nullable
  public ErlangBehaviour getBehaviour() {
    return PsiTreeUtil.getChildOfType(this, ErlangBehaviour.class);
  }

  @Override
  @Nullable
  public ErlangCallbackSpec getCallbackSpec() {
    return PsiTreeUtil.getChildOfType(this, ErlangCallbackSpec.class);
  }

  @Override
  @Nullable
  public ErlangExport getExport() {
    return PsiTreeUtil.getChildOfType(this, ErlangExport.class);
  }

  @Override
  @Nullable
  public ErlangExportTypeAttribute getExportTypeAttribute() {
    return PsiTreeUtil.getChildOfType(this, ErlangExportTypeAttribute.class);
  }

  @Override
  @Nullable
  public ErlangImportDirective getImportDirective() {
    return PsiTreeUtil.getChildOfType(this, ErlangImportDirective.class);
  }

  @Override
  @Nullable
  public ErlangModule getModule() {
    return PsiTreeUtil.getChildOfType(this, ErlangModule.class);
  }

  @Override
  @Nullable
  public ErlangOptionalCallbacks getOptionalCallbacks() {
    return PsiTreeUtil.getChildOfType(this, ErlangOptionalCallbacks.class);
  }

  @Override
  @Nullable
  public ErlangSpecification getSpecification() {
    return PsiTreeUtil.getChildOfType(this, ErlangSpecification.class);
  }

  @Override
  @NotNull
  public PsiElement getOpMinus() {
    return notNullChild(findChildByType(ERL_OP_MINUS));
  }

}
