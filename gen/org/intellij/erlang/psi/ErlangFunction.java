// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunction extends ErlangNamedElement {

  @NotNull
  List<ErlangFunctionClause> getFunctionClauseList();

  @NotNull
  ErlangQAtom getAtomName();

  @NotNull
  String getName();

  int getArity();

}
