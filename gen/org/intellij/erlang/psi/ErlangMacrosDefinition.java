// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangMacrosDefinition extends ErlangNamedElement {

  @Nullable
  ErlangArgumentDefinitionList getArgumentDefinitionList();

  @Nullable
  ErlangMacrosBody getMacrosBody();

  @Nullable
  ErlangMacrosName getMacrosName();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

}
