// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangCaseExpression extends ErlangExpression {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  ErlangExpression getExpression();

  @NotNull
  PsiElement getCase();

  @Nullable
  PsiElement getEnd();

  @Nullable
  PsiElement getOf();

}
