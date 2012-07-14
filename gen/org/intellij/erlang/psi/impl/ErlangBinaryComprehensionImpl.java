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

public class ErlangBinaryComprehensionImpl extends ErlangExpressionImpl implements ErlangBinaryComprehension {

  public ErlangBinaryComprehensionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangBinaryLcExprs getBinaryLcExprs() {
    return findChildByClass(ErlangBinaryLcExprs.class);
  }

  @Override
  @NotNull
  public ErlangExpression getExpression() {
    return findNotNullChildByClass(ErlangExpression.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitBinaryComprehension(this);
    else super.accept(visitor);
  }

}
