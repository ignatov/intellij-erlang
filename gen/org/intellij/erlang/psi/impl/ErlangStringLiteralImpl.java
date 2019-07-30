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

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitStringLiteral(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getString() {
    return notNullChild(findChildByType(ERL_STRING));
  }

  @Override
  public boolean isValidHost() {
    return ErlangPsiImplUtil.isValidHost(this);
  }

  @Override
  @NotNull
  public ErlangStringLiteral updateText(@NotNull String text) {
    return ErlangPsiImplUtil.updateText(this, text);
  }

  @Override
  @NotNull
  public ErlangStringLiteralEscaper createLiteralTextEscaper() {
    return ErlangPsiImplUtil.createLiteralTextEscaper(this);
  }

}
