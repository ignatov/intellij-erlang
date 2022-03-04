// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.psi.*;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.SearchScope;

public class ErlangQVarImpl extends ErlangNamedElementImpl implements ErlangQVar {

  public ErlangQVarImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitQVar(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getVar() {
    return notNullChild(findChildByType(ERL_VAR));
  }

  @Override
  public @NotNull PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  @Override
  public @Nullable PsiReference getReference(@Nullable ErlangMacrosName o) {
    return ErlangPsiImplUtil.getReference(this, o);
  }

  @Override
  public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return ErlangPsiImplUtil.processDeclarations(this, processor, state, lastParent, place);
  }

  @Override
  public @NotNull String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @Override
  public @NotNull PsiElement setName(@NotNull String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @Override
  public @NotNull PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    return ErlangPsiImplUtil.getUseScope(this);
  }

}
