// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.ErlangAfterClauseBody;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangAfterClauseBodyImpl extends ErlangCompositeElementImpl implements ErlangAfterClauseBody {

  public ErlangAfterClauseBodyImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangClauseBody getClauseBody() {
    return findChildByClass(ErlangClauseBody.class);
  }

  @Override
  @Nullable
  public ErlangExpression getExpression() {
    return findChildByClass(ErlangExpression.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitAfterClauseBody(this);
    else super.accept(visitor);
  }

}
