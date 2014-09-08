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

public class ErlangAtomImpl extends ErlangCompositeElementImpl implements ErlangAtom {

  public ErlangAtomImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitAtom(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getAtomName() {
    return findChildByType(ERL_ATOM_NAME);
  }

  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @NotNull
  public ErlangAtom setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

}
