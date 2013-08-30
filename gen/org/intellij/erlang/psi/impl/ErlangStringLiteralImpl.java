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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitStringLiteral(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getString() {
    return findNotNullChildByType(ERL_STRING);
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
