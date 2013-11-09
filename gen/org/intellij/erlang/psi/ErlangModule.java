// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangModuleStub;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.ResolveState;

public interface ErlangModule extends ErlangNamedElement, StubBasedPsiElement<ErlangModuleStub> {

  @Nullable
  ErlangArgumentDefinition getArgumentDefinition();

  @Nullable
  ErlangModelFieldList getModelFieldList();

  @Nullable
  ErlangQAtom getQAtom();

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  PsiElement getNameIdentifier();

  int getTextOffset();

  boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place);

}
