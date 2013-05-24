// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangReceiveExpression extends ErlangExpression {

  @Nullable
  ErlangAfterClause getAfterClause();

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getReceive();

}
