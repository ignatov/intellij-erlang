/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.impl.source.resolve.ResolveCache;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangQAtomBasedReferenceImpl extends PsiReferenceBase<ErlangQAtom> {
  protected final String myReferenceName;

  private static final ResolveCache.AbstractResolver<ErlangQAtomBasedReferenceImpl, PsiElement> MY_RESOLVER =
    new ResolveCache.AbstractResolver<ErlangQAtomBasedReferenceImpl, PsiElement>() {
      @Nullable
      @Override
      public PsiElement resolve(@NotNull ErlangQAtomBasedReferenceImpl base, boolean b) {
        return base.resolveInner();
      }
    };

  @Nullable
  protected abstract PsiElement resolveInner();

  @Nullable
  @Override
  public final PsiElement resolve() {
    return myElement.isValid()
           ? ResolveCache.getInstance(myElement.getProject()).resolveWithCaching(this, MY_RESOLVER, false, false)
           : null;
  }

  public ErlangQAtomBasedReferenceImpl(ErlangQAtom element, TextRange range, String name) {
    super(element, range);
    myReferenceName = name;
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    ErlangPsiImplUtil.renameQAtom(myElement, newElementName);
    return myElement;
  }

  @Override
  public boolean equals(Object o) {
    return this == o || o instanceof ErlangQAtomBasedReferenceImpl && getElement() == ((ErlangQAtomBasedReferenceImpl) o).getElement();
  }

  @Override
  public int hashCode() {
    return getElement().hashCode();
  }
}
