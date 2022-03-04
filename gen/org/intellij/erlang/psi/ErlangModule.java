// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangModuleStub;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

public interface ErlangModule extends ErlangMetaAttribute, ErlangNamedElement, StubBasedPsiElement<ErlangModuleStub> {

  @Nullable
  ErlangArgumentDefinition getArgumentDefinition();

  @Nullable
  ErlangModelFieldList getModelFieldList();

  @Nullable
  ErlangQAtom getQAtom();

  @Nullable
  PsiElement getComma();

  @Nullable
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

  @NotNull String getName();

  @NotNull PsiElement setName(@NotNull String newName);

  @NotNull PsiElement getNameIdentifier();

  int getTextOffset();

  boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place);

}
