/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangQAtom;

public abstract class ErlangAtomBasedReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  protected final String myReferenceName;

  public ErlangAtomBasedReferenceImpl(T element, TextRange range, String name) {
    super(element, range);
    myReferenceName = name;
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    PsiElement atom = myElement.getAtom();
    if (atom != null) {
      atom.replace(ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName));
    }
    return myElement;
  }
}
