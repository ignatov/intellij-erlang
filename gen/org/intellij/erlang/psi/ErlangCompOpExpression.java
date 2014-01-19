// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangCompOpExpression extends ErlangFakeBinaryExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  PsiElement getOpDivEq();

  @Nullable
  PsiElement getOpEqColEq();

  @Nullable
  PsiElement getOpEqDivEq();

  @Nullable
  PsiElement getOpEqEq();

  @Nullable
  PsiElement getOpEqLt();

  @Nullable
  PsiElement getOpGt();

  @Nullable
  PsiElement getOpGtEq();

  @Nullable
  PsiElement getOpLt();

}
