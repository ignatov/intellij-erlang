// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangSpecificationStub;

public interface ErlangSpecification extends ErlangNamedElement, StubBasedPsiElement<ErlangSpecificationStub> {

  @Nullable
  ErlangFunTypeSigs getFunTypeSigs();

  @Nullable
  ErlangFunTypeSigsBraces getFunTypeSigsBraces();

  @Nullable
  ErlangFunTypeSigs getSignature();

  @NotNull
  PsiElement getNameIdentifier();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  int getArity();

}
