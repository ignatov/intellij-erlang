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
import com.intellij.psi.PsiReference;

public class ErlangFunctionCallExpressionImpl extends ErlangExpressionImpl implements ErlangFunctionCallExpression {

  public ErlangFunctionCallExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunctionCallExpression(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangArgumentList getArgumentList() {
    return findNotNullChildByClass(ErlangArgumentList.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getQAtom() {
    return findNotNullChildByClass(ErlangQAtom.class);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

}
