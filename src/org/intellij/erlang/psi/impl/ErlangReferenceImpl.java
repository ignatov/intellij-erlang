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
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;

/**
 * @author ignatov
 */
public class ErlangReferenceImpl<T extends ErlangFunctionCallExpression> extends PsiReferenceBase<T> {

  public ErlangReferenceImpl(@NotNull T element, TextRange range) {
    super(element, range);
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();

    String referenceName = getRangeInElement().substring(myElement.getExpression().getText());
    int size = myElement.getArgumentList().getExpressionList().size();

    return containingFile instanceof ErlangFile ? ((ErlangFile) containingFile).getFunction(referenceName, size) : null;
  }


  @NotNull
  @Override
  public Object[] getVariants() {
    final ArrayList<LookupElement> list = new ArrayList<LookupElement>();

    return ArrayUtil.toObjectArray(list);
  }
}
