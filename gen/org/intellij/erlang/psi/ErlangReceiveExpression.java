// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangReceiveExpression extends ErlangExpression, ErlangClauseOwner {

  @Nullable
  ErlangAfterClause getAfterClause();

  @NotNull
  List<ErlangCrClause> getCrClauseList();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getReceive();

}
