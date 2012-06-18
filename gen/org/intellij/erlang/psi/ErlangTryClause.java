// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTryClause extends ErlangCompositeElement {

  @NotNull
  ErlangClauseBody getClauseBody();

  @NotNull
  ErlangExpression getExpression();

  @Nullable
  ErlangGuard getGuard();

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  ErlangQVar getQVar();

  @Nullable
  PsiElement getWhen();

}
