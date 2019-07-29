// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.SearchScope;

public interface ErlangQVar extends ErlangNamedElement {

  @NotNull
  PsiElement getVar();

  @NotNull
  PsiReference getReference();

  @Nullable
  PsiReference getReference(@Nullable ErlangMacrosName o);

  boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place);

  @NotNull
  String getName();

  @NotNull
  PsiElement setName(@NotNull String newName);

  @NotNull
  PsiElement getNameIdentifier();

  @NotNull
  SearchScope getUseScope();

}
