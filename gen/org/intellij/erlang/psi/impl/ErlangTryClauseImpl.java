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

public class ErlangTryClauseImpl extends ErlangCompositeElementImpl implements ErlangTryClause {

  public ErlangTryClauseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangArgumentDefinition> getArgumentDefinitionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangArgumentDefinition.class);
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
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitTryClause(this);
    else super.accept(visitor);
  }

}
