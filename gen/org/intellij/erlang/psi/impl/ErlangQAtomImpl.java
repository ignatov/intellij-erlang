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

public class ErlangQAtomImpl extends ErlangCompositeElementImpl implements ErlangQAtom {

  public ErlangQAtomImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitQAtom(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangMacros getMacros() {
    return findChildByClass(ErlangMacros.class);
  }

  @Override
  @Nullable
  public ErlangMacrosArg getMacrosArg() {
    return findChildByClass(ErlangMacrosArg.class);
  }

  @Override
  @Nullable
  public PsiElement getAtom() {
    return findChildByType(ERL_ATOM);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

}
