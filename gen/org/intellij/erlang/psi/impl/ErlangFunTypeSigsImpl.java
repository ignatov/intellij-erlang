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
import com.intellij.psi.PsiReference;

public class ErlangFunTypeSigsImpl extends ErlangCompositeElementImpl implements ErlangFunTypeSigs {

  public ErlangFunTypeSigsImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangQAtom> getQAtomList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQAtom.class);
  }

  @Override
  @NotNull
  public List<ErlangTypeSig> getTypeSigList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTypeSig.class);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunTypeSigs(this);
    else super.accept(visitor);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

}
