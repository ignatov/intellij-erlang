/*
 * Copyright 2012-2015 Sergey Ignatov
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
import org.jetbrains.annotations.NotNull;

public class ErlangStringManipulator extends AbstractElementManipulator<ErlangStringLiteral> {
  @Override
  public ErlangStringLiteral handleContentChange(@NotNull ErlangStringLiteral psi, @NotNull TextRange range, String newContent) throws IncorrectOperationException {
    String oldText = psi.getText();
    String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());
    return psi.updateText(newText);
  }

  @NotNull
  @Override
  public TextRange getRangeInElement(@NotNull ErlangStringLiteral element) {
    return getStringTokenRange(element);
  }

  private static TextRange getStringTokenRange(ErlangStringLiteral element) {
    String text = element.getText();
    int length = text.length();
    int start = 0;
    if (start < length && text.charAt(start) == '~') {
      start++;
      if (start < length && "bBsS".indexOf(text.charAt(start)) >= 0) start++;
    }
    int quotes = 0;
    while (start + quotes < length && text.charAt(start + quotes) == '"') quotes++;
    if (quotes >= 3) {
      int contentStart = start + quotes;
      int end = length >= contentStart + quotes && endsWithQuotes(text, quotes) ? length - quotes : length;
      return tripleQuotedContentRange(text, contentStart, end);
    }
    if (start >= length) return TextRange.create(length, length);
    char open = text.charAt(start);
    int contentStart = start + 1;
    int end = length > contentStart && text.charAt(length - 1) == closingDelimiter(open) ? length - 1 : length;
    return TextRange.create(contentStart, end);
  }

  private static boolean endsWithQuotes(String text, int quotes) {
    for (int i = text.length() - quotes; i < text.length(); i++) {
      if (text.charAt(i) != '"') return false;
    }
    return true;
  }

  // The rest of the opening line and the line of the closing quotes are not part of the string (EEP 64)
  private static TextRange tripleQuotedContentRange(String text, int start, int end) {
    int lastLineStart = end;
    while (lastLineStart > start && isBlank(text.charAt(lastLineStart - 1))) lastLineStart--;
    int contentEnd = end;
    if (lastLineStart > start && text.charAt(lastLineStart - 1) == '\n') {
      contentEnd = lastLineStart - 1;
      if (contentEnd > start && text.charAt(contentEnd - 1) == '\r') contentEnd--;
    }
    int lineBreak = start;
    while (lineBreak < end && isBlank(text.charAt(lineBreak))) lineBreak++;
    if (lineBreak < end && text.charAt(lineBreak) == '\n') start = lineBreak + 1;
    return TextRange.create(start, Math.max(start, contentEnd));
  }

  private static boolean isBlank(char c) {
    return c == ' ' || c == '\t' || c == '\r';
  }

  private static char closingDelimiter(char open) {
    return switch (open) {
      case '(' -> ')';
      case '[' -> ']';
      case '{' -> '}';
      case '<' -> '>';
      default -> open;
    };
  }
}
