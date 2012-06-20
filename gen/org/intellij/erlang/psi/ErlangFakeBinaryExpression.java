// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFakeBinaryExpression extends ErlangExpression {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @NotNull
  ErlangExpression getLeft();

  @Nullable
  ErlangExpression getRight();

}
