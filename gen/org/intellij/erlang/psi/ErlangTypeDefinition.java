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
  ErlangTopType getTopType();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

  int getArity();

}
