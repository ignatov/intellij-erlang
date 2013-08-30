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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitAttribute(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangAtomAttribute getAtomAttribute() {
    return findChildByClass(ErlangAtomAttribute.class);
  }

  @Override
  @Nullable
  public ErlangBehaviour getBehaviour() {
    return findChildByClass(ErlangBehaviour.class);
  }

  @Override
  @Nullable
  public ErlangCallbackSpec getCallbackSpec() {
    return findChildByClass(ErlangCallbackSpec.class);
  }

  @Override
  @Nullable
  public ErlangExport getExport() {
    return findChildByClass(ErlangExport.class);
  }

  @Override
  @Nullable
  public ErlangExportTypeAttribute getExportTypeAttribute() {
    return findChildByClass(ErlangExportTypeAttribute.class);
  }

  @Override
  @Nullable
  public ErlangImportDirective getImportDirective() {
    return findChildByClass(ErlangImportDirective.class);
  }

  @Override
  @Nullable
  public ErlangModule getModule() {
    return findChildByClass(ErlangModule.class);
  }

  @Override
  @Nullable
  public ErlangSpecification getSpecification() {
    return findChildByClass(ErlangSpecification.class);
  }

}
