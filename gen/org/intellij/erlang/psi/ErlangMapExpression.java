// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangMapExpression extends ErlangExpression {

  @NotNull
  ErlangExpression getExpression();

  @Nullable
  ErlangMapEntries getMapEntries();

  @NotNull
  PsiElement getCurlyLeft();

  @Nullable
  PsiElement getCurlyRight();

  @NotNull
  PsiElement getRadix();

}
