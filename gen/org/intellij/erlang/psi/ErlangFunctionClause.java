// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface ErlangFunctionClause extends ErlangCompositeElement {

  @NotNull
  ErlangArgumentDefinitionList getArgumentDefinitionList();

  @Nullable
  ErlangClauseBody getClauseBody();

  @Nullable
  ErlangClauseGuard getClauseGuard();

  @NotNull
  ErlangQAtom getQAtom();

}
