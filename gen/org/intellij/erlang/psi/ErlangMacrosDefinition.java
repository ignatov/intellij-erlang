// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangMacrosDefinition extends ErlangNamedElement {

  @NotNull
  List<ErlangExpression> getExpressionList();

  @Nullable
  ErlangMacrosName getMacrosName();

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @NotNull
  List<ErlangQVar> getQVarList();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

}
