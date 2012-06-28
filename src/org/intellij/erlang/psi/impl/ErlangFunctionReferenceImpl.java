/*
 * Copyright 2011-2011 Gregory Shrago
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

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangFunctionReferenceImpl<T extends ErlangQAtom> extends PsiReferenceBase<T> {
  private String myReferenceName;
  private int myArity;

  public ErlangFunctionReferenceImpl(@NotNull T element, TextRange range, String name, int arity) {
    super(element, range);
    myReferenceName = name;
    myArity = arity;
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunction(myReferenceName, myArity) : null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    List<LookupElement> lookupElements = ErlangPsiImplUtil.getFunctionLookupElements(myElement.getContainingFile());
    return ArrayUtil.toObjectArray(lookupElements);
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    try {
      myElement.replace(ErlangElementFactory.createQAtomFromText(getElement().getProject(), newElementName));
    } catch (Exception e) {
      throw new IncorrectOperationException("Incorrect function name");
    }
    return myElement;
  }
}
