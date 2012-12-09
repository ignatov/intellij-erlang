// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangType extends ErlangCompositeElement {

  @Nullable
  ErlangBinaryType getBinaryType();

  @NotNull
  List<ErlangFieldType> getFieldTypeList();

  @Nullable
  ErlangFunType100T getFunType100T();

  @NotNull
  List<ErlangIntType> getIntTypeList();

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  ErlangQVar getQVar();

  @Nullable
  ErlangTopType getTopType();

  @Nullable
  ErlangTypeRef getTypeRef();

  @Nullable
  PsiElement getFun();

}
