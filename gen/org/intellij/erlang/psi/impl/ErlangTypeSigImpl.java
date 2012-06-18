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

public class ErlangTypeSigImpl extends ErlangCompositeElementImpl implements ErlangTypeSig {

  public ErlangTypeSigImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangFunType getFunType() {
    return findNotNullChildByClass(ErlangFunType.class);
  }

  @Override
  @NotNull
  public List<ErlangTypeGuard> getTypeGuardList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangTypeGuard.class);
  }

  @Override
  @Nullable
  public PsiElement getWhen() {
    return findChildByType(ERL_WHEN);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTypeSig(this);
    else super.accept(visitor);
  }

}
