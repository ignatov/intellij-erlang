// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangTypeDefinitionStub;

public interface ErlangTypeDefinition extends ErlangNamedElement, StubBasedPsiElement<ErlangTypeDefinitionStub> {

  @Nullable
  ErlangArgumentDefinitionList getArgumentDefinitionList();

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  ErlangType getType();

  @Nullable
  PsiElement getColonColon();

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

  int getArity();

}
