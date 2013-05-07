// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangGuard;
import org.intellij.erlang.psi.ErlangIfClause;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangIfClauseImpl extends ErlangCompositeElementImpl implements ErlangIfClause {

  public ErlangIfClauseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangClauseBody getClauseBody() {
    return findChildByClass(ErlangClauseBody.class);
  }

  @Override
  @Nullable
  public ErlangGuard getGuard() {
    return findChildByClass(ErlangGuard.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitIfClause(this);
    else super.accept(visitor);
  }

}
