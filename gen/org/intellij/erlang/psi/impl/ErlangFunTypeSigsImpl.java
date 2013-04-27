// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangFunTypeSigsImpl extends ErlangCompositeElementImpl implements ErlangFunTypeSigs {

  public ErlangFunTypeSigsImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangModuleRef getModuleRef() {
    return findChildByClass(ErlangModuleRef.class);
  }

  @Override
  @NotNull
  public ErlangSpecFun getSpecFun() {
    return findNotNullChildByClass(ErlangSpecFun.class);
  }

  @Override
  @NotNull
  public List<ErlangTypeSig> getTypeSigList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTypeSig.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunTypeSigs(this);
    else super.accept(visitor);
  }

}
