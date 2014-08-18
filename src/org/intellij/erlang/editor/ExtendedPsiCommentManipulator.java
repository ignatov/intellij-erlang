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

package org.intellij.erlang.editor;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.impl.source.resolve.reference.impl.manipulators.PsiCommentManipulator;

public class ExtendedPsiCommentManipulator extends PsiCommentManipulator {
  @Override
  public TextRange getRangeInElement(PsiComment element) {
    String text = element.getText();
    if (text.startsWith("%%%")) return new TextRange(3, element.getTextLength());
    if (text.startsWith("%%")) return new TextRange(2, element.getTextLength());
    if (text.startsWith("%")) return new TextRange(1, element.getTextLength());
    return super.getRangeInElement(element);
  }
}
