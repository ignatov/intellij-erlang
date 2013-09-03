// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunctionClause extends ErlangCompositeElement {

  @NotNull
  ErlangArgumentDefinitionList getArgumentDefinitionList();

  @NotNull
  ErlangClauseBody getClauseBody();

  @Nullable
  ErlangClauseGuard getClauseGuard();

  @NotNull
  ErlangQAtom getQAtom();

}
