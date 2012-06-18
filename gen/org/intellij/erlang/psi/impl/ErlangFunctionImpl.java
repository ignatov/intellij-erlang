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

public class ErlangFunctionImpl extends ErlangCompositeElementImpl implements ErlangFunction {

  public ErlangFunctionImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangFunctionClause> getFunctionClauseList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangFunctionClause.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitFunction(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public ErlangQAtom getAtomName() {
    List<ErlangFunctionClause> p1 = getFunctionClauseList();
    ErlangFunctionClause p2 = p1.get(0);
    return p2.getQAtom();
  }

}
