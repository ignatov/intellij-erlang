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

public class ErlangRuleClauseImpl extends ErlangCompositeElementImpl implements ErlangRuleClause {

  public ErlangRuleClauseImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitRuleClause(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangArgumentList getArgumentList() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangArgumentList.class));
  }

  @Override
  @Nullable
  public ErlangClauseGuard getClauseGuard() {
    return PsiTreeUtil.getChildOfType(this, ErlangClauseGuard.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangQAtom.class));
  }

  @Override
  @NotNull
  public ErlangRuleBody getRuleBody() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, ErlangRuleBody.class));
  }

}
