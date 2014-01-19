// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangMultiplicativeExpression extends ErlangFakeBinaryExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  PsiElement getOpArDiv();

  @Nullable
  PsiElement getOpArMul();

  @Nullable
  PsiElement getAnd();

  @Nullable
  PsiElement getBand();

  @Nullable
  PsiElement getDiv();

  @Nullable
  PsiElement getRem();

}
