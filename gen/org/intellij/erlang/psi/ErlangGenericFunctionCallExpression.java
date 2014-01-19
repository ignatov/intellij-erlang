// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangGenericFunctionCallExpression extends ErlangExpression {

  @NotNull
  ErlangArgumentList getArgumentList();

  @Nullable
  ErlangMacros getMacros();

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @NotNull
  List<ErlangQVar> getQVarList();

  @Nullable
  PsiElement getColon();

}
