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

public class ErlangFunTypeSigsImpl extends ErlangCompositeElementImpl implements ErlangFunTypeSigs {

  public ErlangFunTypeSigsImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunTypeSigs(this);
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
  @NotNull
  public ErlangSpecFun getSpecFun() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangSpecFun.class));
  }

  @Override
  @NotNull
  public List<ErlangTypeSig> getTypeSigList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTypeSig.class);
  }

  @Override
  @Nullable
  public PsiElement getColon() {
    return findChildByType(ERL_COLON);
  }

  @Override
  @Nullable
  public PsiElement getColonColon() {
    return findChildByType(ERL_COLON_COLON);
  }

}
