// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangAdditiveExpression extends ErlangFakeBinaryExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  PsiElement getOpMinus();

  @Nullable
  PsiElement getOpPlus();

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
