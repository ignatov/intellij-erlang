// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.intellij.erlang.ErlangStringLiteralEscaper;
import org.intellij.erlang.psi.ErlangStringLiteral;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.ErlangTypes.ERL_STRING;

public class ErlangStringLiteralImpl extends ErlangExpressionImpl implements ErlangStringLiteral {

  public ErlangStringLiteralImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public PsiElement getString() {
    return findNotNullChildByType(ERL_STRING);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitStringLiteral(this);
    else super.accept(visitor);
  }

  public boolean isValidHost() {
    return ErlangPsiImplUtil.isValidHost(this);
  }

  public ErlangStringLiteral updateText(String text) {
    return ErlangPsiImplUtil.updateText(this, text);
  }

  @NotNull
  public ErlangStringLiteralEscaper createLiteralTextEscaper() {
    return ErlangPsiImplUtil.createLiteralTextEscaper(this);
  }

}
