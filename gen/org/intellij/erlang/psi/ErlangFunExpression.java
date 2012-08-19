// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunExpression extends ErlangExpression {

  @NotNull
  List<ErlangFunClause> getFunClauseList();

  @Nullable
  ErlangFunctionWithArity getFunctionWithArity();

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  PsiElement getEnd();

  @NotNull
  PsiElement getFun();

}
