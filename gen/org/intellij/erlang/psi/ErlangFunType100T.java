// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangFunType100T extends ErlangType {

  @Nullable
  ErlangTopTypeClause getTopTypeClause();

  @NotNull
  List<ErlangType> getTypeList();

  @Nullable
  PsiElement getDotDotDot();

  @NotNull
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

}
