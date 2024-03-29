// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangMaybeExpression extends ErlangExpression, ErlangClauseOwner {

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  ErlangMaybeMatchExprs getMaybeMatchExprs();

  @Nullable
  PsiElement getElse();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getMaybe();

}
