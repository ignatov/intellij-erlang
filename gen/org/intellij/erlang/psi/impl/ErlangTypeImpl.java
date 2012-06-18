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

  @Override
  @Nullable
  public ErlangBinaryType getBinaryType() {
    return findChildByClass(ErlangBinaryType.class);
  }

  @Override
  @Nullable
  public ErlangFieldTypes getFieldTypes() {
    return findChildByClass(ErlangFieldTypes.class);
  }

  @Override
  @Nullable
  public ErlangFunType100T getFunType100T() {
    return findChildByClass(ErlangFunType100T.class);
  }

  @Override
  @Nullable
  public ErlangIntType getIntType() {
    return findChildByClass(ErlangIntType.class);
  }

  @Override
  @NotNull
  public List<ErlangQAtom> getQAtomList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public ErlangQVar getQVar() {
    return findChildByClass(ErlangQVar.class);
  }

  @Override
  @Nullable
  public ErlangTopType getTopType() {
    return findChildByClass(ErlangTopType.class);
  }

  @Override
  @Nullable
  public ErlangTopTypes getTopTypes() {
    return findChildByClass(ErlangTopTypes.class);
  }

  @Override
  @Nullable
  public PsiElement getFun() {
    return findChildByType(ERL_FUN);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitType(this);
    else super.accept(visitor);
  }

}
