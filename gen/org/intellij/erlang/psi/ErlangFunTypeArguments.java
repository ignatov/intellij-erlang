// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunTypeArguments extends ErlangCompositeElement {

  @NotNull
  List<ErlangType> getTypeList();

  @NotNull
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

}
