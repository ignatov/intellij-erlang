// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangQualifiedExpression extends ErlangExpression {

  @NotNull
  List<ErlangQAtom> getQAtomList();

  @NotNull
  PsiElement getDot();

}
