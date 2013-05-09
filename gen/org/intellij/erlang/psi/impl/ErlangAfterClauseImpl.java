// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.ErlangAfterClause;
import org.intellij.erlang.psi.ErlangAfterClauseBody;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.intellij.erlang.ErlangTypes.ERL_AFTER;

public class ErlangAfterClauseImpl extends ErlangCompositeElementImpl implements ErlangAfterClause {

  public ErlangAfterClauseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangAfterClauseBody getAfterClauseBody() {
    return findChildByClass(ErlangAfterClauseBody.class);
  }

  @Override
  @NotNull
  public PsiElement getAfter() {
    return findNotNullChildByType(ERL_AFTER);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitAfterClause(this);
    else super.accept(visitor);
  }

}
