// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFunctionClauseImpl extends ErlangCompositeElementImpl implements ErlangFunctionClause {

  public ErlangFunctionClauseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public ErlangArgumentDefinitionList getArgumentDefinitionList() {
    return findNotNullChildByClass(ErlangArgumentDefinitionList.class);
  }

  @Override
  @Nullable
  public ErlangClauseBody getClauseBody() {
    return findChildByClass(ErlangClauseBody.class);
  }

  @Override
  @Nullable
  public ErlangClauseGuard getClauseGuard() {
    return findChildByClass(ErlangClauseGuard.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return findNotNullChildByClass(ErlangQAtom.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunctionClause(this);
    else super.accept(visitor);
  }

}
