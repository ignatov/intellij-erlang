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

public class ErlangFunExpressionImpl extends ErlangExpressionImpl implements ErlangFunExpression {

  public ErlangFunExpressionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangFunClause> getFunClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangFunClause.class);
  }

  @Override
  @NotNull
  public List<ErlangQAtom> getQAtomList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangQAtom.class);
  }

  @Override
  @Nullable
  public PsiElement getEnd() {
    return findChildByType(ERL_END);
  }

  @Override
  @NotNull
  public PsiElement getFun() {
    return findNotNullChildByType(ERL_FUN);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(ERL_INTEGER);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunExpression(this);
    else super.accept(visitor);
  }

}
