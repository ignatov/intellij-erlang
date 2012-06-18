// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangTypeSig extends ErlangCompositeElement {

  @NotNull
  ErlangFunType getFunType();

  @NotNull
  List<ErlangTypeGuard> getTypeGuardList();

  @Nullable
  PsiElement getWhen();

}
