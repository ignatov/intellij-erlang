// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangBinaryType extends ErlangType {

  @NotNull
  List<ErlangType> getTypeList();

  @Nullable
  PsiElement getBinEnd();

  @NotNull
  PsiElement getBinStart();

  @Nullable
  PsiElement getComma();

}
