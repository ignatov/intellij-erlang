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

package org.intellij.erlang.intention;

import com.intellij.codeInsight.intention.impl.BaseIntentionAction;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangBaseNamedElementIntention extends BaseIntentionAction {
  private final String myFamilyName;
  private final String myText;

  protected ErlangBaseNamedElementIntention(String familyName, String text) {
    myFamilyName = familyName;
    myText = text;
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return myFamilyName;
  }

  @NotNull
  @Override
  public String getText() {
    return myText;
  }

  @Nullable
  protected static <T extends ErlangNamedElement> T findElement(PsiFile file, int offset, Class<T> type) {
    PsiElement element = file.findElementAt(offset);
    T res = PsiTreeUtil.getParentOfType(element, type);
    if (res == null) return null;

    PsiElement name = res.getNameIdentifier();
    TextRange textRange = name != null ? name.getTextRange() : null;
    if (textRange == null || textRange.getEndOffset() < offset) return null;

    return res;
  }
}
