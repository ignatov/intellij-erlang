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

public class ErlangTypeImpl extends ErlangCompositeElementImpl implements ErlangType {

  public ErlangTypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitType(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangBinaryType getBinaryType() {
    return findChildByClass(ErlangBinaryType.class);
  }

  @Override
  @NotNull
  public List<ErlangFieldType> getFieldTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangFieldType.class);
  }

  @Override
  @Nullable
  public ErlangFunType100T getFunType100T() {
    return findChildByClass(ErlangFunType100T.class);
  }

  @Override
  @NotNull
  public List<ErlangIntType> getIntTypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangIntType.class);
  }

  @Override
  @Nullable
  public ErlangModuleRef getModuleRef() {
    return findChildByClass(ErlangModuleRef.class);
  }

  @Override
  @Nullable
  public ErlangQVar getQVar() {
    return findChildByClass(ErlangQVar.class);
  }

  @Override
  @Nullable
  public ErlangRecordRef getRecordRef() {
    return findChildByClass(ErlangRecordRef.class);
  }

  @Override
  @Nullable
  public ErlangTopType getTopType() {
    return findChildByClass(ErlangTopType.class);
  }

  @Override
  @Nullable
  public ErlangTypeRef getTypeRef() {
    return findChildByClass(ErlangTypeRef.class);
  }

  @Override
  @Nullable
  public PsiElement getFun() {
    return findChildByType(ERL_FUN);
  }

}
