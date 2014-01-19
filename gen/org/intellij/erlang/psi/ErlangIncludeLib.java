// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangIncludeLibStub;

public interface ErlangIncludeLib extends ErlangCompositeElement, StubBasedPsiElement<ErlangIncludeLibStub> {

  @Nullable
  ErlangIncludeString getIncludeString();

  @NotNull
  PsiElement getOpMinus();

  @Nullable
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

  @Nullable
  ErlangIncludeString getIncludeStringSafe();

}
