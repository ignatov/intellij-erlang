// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static org.intellij.erlang.ErlangTypes.*;
import org.intellij.erlang.stubs.ErlangFunctionStub;
import org.intellij.erlang.psi.*;
import com.intellij.navigation.ItemPresentation;
import javax.swing.Icon;
import com.intellij.psi.stubs.IStubElementType;

public class ErlangFunctionImpl extends ErlangNamedStubbedPsiElementBase<ErlangFunctionStub> implements ErlangFunction {

  public ErlangFunctionImpl(@NotNull ErlangFunctionStub stub, @NotNull IStubElementType type) {
    super(stub, type);
  }

  public ErlangFunctionImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull ErlangVisitor visitor) {
    visitor.visitFunction(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) accept((ErlangVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<ErlangFunctionClause> getFunctionClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangFunctionClause.class);
  }

  @Override
  @NotNull
  public ErlangQAtom getAtomName() {
    List<ErlangFunctionClause> p1 = getFunctionClauseList();
    ErlangFunctionClause p2 = p1.get(0);
    return p2.getQAtom();
  }

  @Override
  @NotNull
  public ErlangFunctionClause getFirstClause() {
    List<ErlangFunctionClause> p1 = getFunctionClauseList();
    return p1.get(0);
  }

  @Override
  @NotNull
  public String getName() {
    return ErlangPsiImplUtil.getName(this);
  }

  @Override
  @NotNull
  public PsiElement setName(@NotNull String newName) {
    return ErlangPsiImplUtil.setName(this, newName);
  }

  @Override
  public int getArity() {
    return ErlangPsiImplUtil.getArity(this);
  }

  @Override
  @NotNull
  public PsiElement getNameIdentifier() {
    return ErlangPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  @NotNull
  public ItemPresentation getPresentation() {
    return ErlangPsiImplUtil.getPresentation(this);
  }

  @Override
  @NotNull
  public Icon getIcon(int flags) {
    return ErlangPsiImplUtil.getIcon(this, flags);
  }

  @Override
  public boolean isExported() {
    return ErlangPsiImplUtil.isExported(this);
  }

  @Override
  @Nullable
  public ErlangSpecification findSpecification() {
    return ErlangPsiImplUtil.findSpecification(this);
  }

}
