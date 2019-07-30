// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangMacrosDefinitionStub;

public interface ErlangMacrosDefinition extends ErlangNamedElement, StubBasedPsiElement<ErlangMacrosDefinitionStub> {

  @Nullable
  ErlangArgumentDefinitionList getArgumentDefinitionList();

  @Nullable
  ErlangMacrosBody getMacrosBody();

  @Nullable
  ErlangMacrosName getMacrosName();

  @Nullable
  PsiElement getComma();

  @NotNull
  PsiElement getOpMinus();

  @Nullable
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(@NotNull String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

}
