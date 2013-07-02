// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangCaseExpression extends ErlangExpression, ErlangClauseOwner {

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
