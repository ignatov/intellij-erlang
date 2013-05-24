// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangTryExpression extends ErlangExpression {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  ErlangTryCatch getTryCatch();

  @Nullable
  ErlangTryExpressionsClause getTryExpressionsClause();

  @Nullable
  PsiElement getOf();

  @NotNull
  PsiElement getTry();

}
