// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangCrClauseImpl extends ErlangCompositeElementImpl implements ErlangCrClause {

  public ErlangCrClauseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinition getArgumentDefinition() {
    return findChildByClass(ErlangArgumentDefinition.class);
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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitCrClause(this);
    else super.accept(visitor);
  }

}
