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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangRecordReferenceImpl<T extends ErlangCompositeElement> extends PsiReferenceBase<T> {
  private String myReferenceName;

  public ErlangRecordReferenceImpl(@NotNull T element, TextRange range, String name) {
    super(element, range);
    myReferenceName = name;
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();
    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getRecord(myReferenceName) : null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(myElement.getContainingFile()));
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    return myElement;
  }
}
