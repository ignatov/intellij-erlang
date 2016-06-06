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

public class ErlangCrClauseImpl extends ErlangCompositeElementImpl implements ErlangCrClause {

  public ErlangCrClauseImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitCrClause(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinition getArgumentDefinition() {
    return PsiTreeUtil.getChildOfType(this, ErlangArgumentDefinition.class);
  }

  @Override
  @Nullable
  public ErlangClauseBody getClauseBody() {
    return PsiTreeUtil.getChildOfType(this, ErlangClauseBody.class);
  }

  @Override
  @Nullable
  public ErlangClauseGuard getClauseGuard() {
    return PsiTreeUtil.getChildOfType(this, ErlangClauseGuard.class);
  }

}
