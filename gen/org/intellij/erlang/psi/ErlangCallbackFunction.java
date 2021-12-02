// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangCallbackFunctionStub;
import com.intellij.psi.PsiReference;

public interface ErlangCallbackFunction extends ErlangCompositeElement, StubBasedPsiElement<ErlangCallbackFunctionStub> {

  @NotNull
  ErlangQAtom getQAtom();

  @Nullable
  PsiElement getOpArDiv();

  @Nullable
  PsiElement getInteger();

  @NotNull PsiReference getReference();

  @Nullable PsiReference getReference(@Nullable ErlangMacrosName o);

}
