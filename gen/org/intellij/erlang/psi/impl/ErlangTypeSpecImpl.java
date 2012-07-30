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

public class ErlangTypeSpecImpl extends ErlangCompositeElementImpl implements ErlangTypeSpec {

  public ErlangTypeSpecImpl(ASTNode node) {
    super(node);
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
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTypeSpec(this);
    else super.accept(visitor);
  }

}
