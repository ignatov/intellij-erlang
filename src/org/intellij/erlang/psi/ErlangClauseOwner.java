package org.intellij.erlang.psi;

import org.jetbrains.annotations.NotNull;
import java.util.List;

public interface ErlangClauseOwner {
  @NotNull
  List<ErlangCrClause> getCrClauseList();
}
