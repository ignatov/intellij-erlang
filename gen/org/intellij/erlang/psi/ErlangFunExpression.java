// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunExpression extends ErlangExpression {

  @NotNull
  List<ErlangFunClause> getFunClauseList();

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getFun();

  @Nullable
  PsiElement getInteger();

}
