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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangStringLiteral;

public class ErlangStringManipulator extends AbstractElementManipulator<ErlangStringLiteral> {
  @Override
  public ErlangStringLiteral handleContentChange(ErlangStringLiteral psi, TextRange range, String newContent) throws IncorrectOperationException {
    final String oldText = psi.getText();
    final String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());
    return psi.updateText(newText);
  }

  @Override
  public TextRange getRangeInElement(final ErlangStringLiteral element) {
    return getStringTokenRange(element);
  }

  public static TextRange getStringTokenRange(final ErlangStringLiteral element) {
    return TextRange.from(1, element.getTextLength() - 2);
  }
}
