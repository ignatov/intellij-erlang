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

package org.intellij.erlang;

import com.intellij.codeInsight.TargetElementEvaluatorEx;
import com.intellij.codeInsight.TargetElementUtilBase;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangTargetElementEvaluator implements TargetElementEvaluatorEx {
  @Override
  public boolean isIdentifierPart(PsiFile element, CharSequence text, int offset) {
    return false;
  }

  @Override
  public boolean includeSelfInGotoImplementation(@NotNull PsiElement element) {
    return false;
  }

  @Nullable
  @Override
  public PsiElement getElementByReference(PsiReference ref, int flags) {
    if ((flags & TargetElementUtilBase.ELEMENT_NAME_ACCEPTED) == 0) return null;
    PsiElement element = ref.getElement();
    return element instanceof ErlangQAtom && ref.resolve() == null ? element : null;
  }
}
