// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.psi.ErlangGenericFunctionCallExpression;
import org.intellij.erlang.psi.ErlangMacrosCall;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangMacrosCallImpl extends ErlangCompositeElementImpl implements ErlangMacrosCall {

  public ErlangMacrosCallImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangGenericFunctionCallExpression getGenericFunctionCallExpression() {
    return findChildByClass(ErlangGenericFunctionCallExpression.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitMacrosCall(this);
    else super.accept(visitor);
  }

}
