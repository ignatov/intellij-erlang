// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangBinUnitType extends ErlangType {

  @NotNull
  ErlangIntType getIntType();

  @NotNull
  List<ErlangQVar> getQVarList();

  @NotNull
  PsiElement getColon();

  @NotNull
  PsiElement getOpArMul();

}
