// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangFunTypeSigs extends ErlangCompositeElement {

  @Nullable
  ErlangModuleRef getModuleRef();

  @NotNull
  ErlangSpecFun getSpecFun();

  @NotNull
  List<ErlangTypeSig> getTypeSigList();

}
