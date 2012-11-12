// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangArgumentList extends ErlangCompositeElement {

  @NotNull
  List<ErlangClauseGuard> getClauseGuardList();

  @NotNull
  List<ErlangExpression> getExpressionList();

}
