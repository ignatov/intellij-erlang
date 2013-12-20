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

package org.intellij.erlang.editor;

import com.intellij.codeInsight.hint.ImplementationTextSelectioner;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.jetbrains.annotations.NotNull;

public class ErlangImplementationTextSelectioner implements ImplementationTextSelectioner {
  @Override
  public int getTextStartOffset(@NotNull PsiElement psiElement) {
    return getTextRange(psiElement).getStartOffset();
  }

  @Override
  public int getTextEndOffset(@NotNull PsiElement psiElement) {
    return getTextRange(psiElement).getEndOffset();
  }

  private static TextRange getTextRange(PsiElement psiElement) {
    @SuppressWarnings("unchecked")
    PsiElement function = PsiTreeUtil.getParentOfType(psiElement, ErlangFunctionClause.class, ErlangAttribute.class);
    PsiElement element = function == null ? psiElement : function;
    return element.getTextRange();
  }
}
