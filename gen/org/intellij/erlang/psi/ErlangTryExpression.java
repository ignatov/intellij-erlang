// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTryExpression extends ErlangExpression, ErlangClauseOwner {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  ErlangTryClauses getTryClauses();

  @NotNull
  List<ErlangTryExpressionsClause> getTryExpressionsClauseList();

  @Nullable
  PsiElement getAfter();

  @Nullable
  PsiElement getCatch();

  @Nullable
  PsiElement getEnd();

  @Nullable
  PsiElement getOf();

  @NotNull
  PsiElement getTry();

}
