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

public class ErlangIfExpressionImpl extends ErlangExpressionImpl implements ErlangIfExpression {

  public ErlangIfExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitIfExpression(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangIfClause> getIfClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangIfClause.class);
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

  @Override
  @NotNull
  public PsiElement getIf() {
    return findNotNullChildByType(ERL_IF);
  }

}
