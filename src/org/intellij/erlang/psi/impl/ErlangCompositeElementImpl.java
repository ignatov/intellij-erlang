/*
 * Copyright 2012 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.ErlangTypes.ERL_DOT;

/**
 * @author ignatov
 */
public class ErlangCompositeElementImpl extends ASTWrapperPsiElement implements ErlangCompositeElement {
  private PsiReference myReference = null;

  public ErlangCompositeElementImpl(ASTNode node) {
    super(node);
  }

  @Override
  public String toString() {
    return getNode().getElementType().toString();
  }

  @Override
  public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    if (!processor.execute(this, state)) {
      return false;
    } else {
      return ResolveUtil.processChildren(this, processor, state, lastParent, place);
    }
  }

  @Override
  public void delete() throws IncorrectOperationException { // todo: move to more appropriate place
    if (!(this instanceof ErlangFunction) && !(this instanceof ErlangRecordDefinition)) return;

    PsiElement nextSibling = getNextSibling();
    if (nextSibling instanceof LeafPsiElement && nextSibling.getNode().getElementType() == ERL_DOT) {
      nextSibling.delete();
    }
    super.delete();
  }

  @Override
  public PsiReference getReference() {
    return myReference;
  }

  @Override
  public void setReference(PsiReference reference) {
    myReference = reference;
  }
}
