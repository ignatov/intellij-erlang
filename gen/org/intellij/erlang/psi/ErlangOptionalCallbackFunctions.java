// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangOptionalCallbackFunctions extends ErlangCompositeElement {

  @NotNull
  List<ErlangCallbackFunction> getCallbackFunctionList();

  @NotNull
  PsiElement getBracketLeft();

  @Nullable
  PsiElement getBracketRight();

}
