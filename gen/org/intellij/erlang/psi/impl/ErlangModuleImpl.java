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
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.ResolveState;

public class ErlangModuleImpl extends ErlangNamedElementImpl implements ErlangModule {

  public ErlangModuleImpl(ASTNode node) {
    super(node);
  }

  @Override
  @Nullable
  public ErlangArgumentDefinition getArgumentDefinition() {
    return findChildByClass(ErlangArgumentDefinition.class);
  }

  @Override
  @Nullable
  public ErlangModelFieldList getModelFieldList() {
    return findChildByClass(ErlangModelFieldList.class);
  }

  @Override
  @Nullable
  public ErlangQAtom getQAtom() {
    return findChildByClass(ErlangQAtom.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitModule(this);
    else super.accept(visitor);
  }

  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @NotNull
  public PsiElement setName(String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  public int getTextOffset() {
    return ErlangPsiImplUtil.getTextOffset(this);
  }

  public boolean processDeclarations(PsiScopeProcessor processor, ResolveState state, PsiElement lastParent, PsiElement place) {
    return ErlangPsiImplUtil.processDeclarations(this, processor, state, lastParent, place);
  }

}
