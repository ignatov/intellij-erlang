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
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.ResolveState;

public class ErlangQVarImpl extends ErlangCompositeElementImpl implements ErlangQVar {

  public ErlangQVarImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public PsiElement getVar() {
    return findChildByType(ERL_VAR);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitQVar(this);
    else super.accept(visitor);
  }

  @Nullable
  public Object resolve() {
    return ErlangPsiImplUtil.resolve(this);
  }

  @Nullable
  public PsiReference getReference() {
    return ErlangPsiImplUtil.getReference(this);
  }

  public boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place) {
    return ErlangPsiImplUtil.processDeclarations(this, processor, state, lastParent, place);
  }

}
