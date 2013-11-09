// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangFunctionStub;
import com.intellij.navigation.ItemPresentation;
import javax.swing.Icon;

public interface ErlangFunction extends ErlangNamedElement, StubBasedPsiElement<ErlangFunctionStub> {

  @NotNull
  List<ErlangFunctionClause> getFunctionClauseList();

  @NotNull
  ErlangQAtom getAtomName();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  int getArity();

  @NotNull
  PsiElement getNameIdentifier();

  @NotNull
  ItemPresentation getPresentation();

  @NotNull
  Icon getIcon(int flags);

  boolean isExported();

}
