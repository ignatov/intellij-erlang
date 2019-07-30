// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangRecordDefinitionStub;

public interface ErlangRecordDefinition extends ErlangNamedElement, StubBasedPsiElement<ErlangRecordDefinitionStub> {

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  ErlangTypedRecordFields getTypedRecordFields();

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
