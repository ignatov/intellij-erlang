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
import org.intellij.erlang.ErlangStringLiteralEscaper;

public class ErlangStringLiteralImpl extends ErlangExpressionImpl implements ErlangStringLiteral {

  public ErlangStringLiteralImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitStringLiteral(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getString() {
    return findChildByType(ERL_STRING);
  }

  @Override
  @Nullable
  public PsiElement getTripleQuotedString() {
    return findChildByType(ERL_TRIPLE_QUOTED_STRING);
  }

  @Override
  public boolean isValidHost() {
    return ErlangPsiImplUtil.isValidHost(this);
  }

  @Override
  public @NotNull ErlangStringLiteral updateText(@NotNull String text) {
    return ErlangPsiImplUtil.updateText(this, text);
  }

  @Override
  public @NotNull ErlangStringLiteralEscaper createLiteralTextEscaper() {
    return ErlangPsiImplUtil.createLiteralTextEscaper(this);
  }

}
