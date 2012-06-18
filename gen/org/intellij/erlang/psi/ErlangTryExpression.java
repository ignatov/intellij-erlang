// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTryExpression extends ErlangExpression {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  ErlangTryCatch getTryCatch();

  @Nullable
  PsiElement getOf();

  @NotNull
  PsiElement getTry();

}
