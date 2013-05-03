// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangAdditiveExpression extends ErlangFakeBinaryExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  PsiElement getBor();

  @Nullable
  PsiElement getBsl();

  @Nullable
  PsiElement getBsr();

  @Nullable
  PsiElement getBxor();

  @Nullable
  PsiElement getOr();

  @Nullable
  PsiElement getXor();

}
