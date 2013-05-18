// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangModelFieldList;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangModelFieldListImpl extends ErlangCompositeElementImpl implements ErlangModelFieldList {

  public ErlangModelFieldListImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public List<ErlangArgumentDefinition> getArgumentDefinitionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangArgumentDefinition.class);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof ErlangVisitor) ((ErlangVisitor)visitor).visitModelFieldList(this);
    else super.accept(visitor);
  }

}
