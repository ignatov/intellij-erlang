/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.application.options.CodeStyle;
import com.intellij.codeInsight.editorActions.JoinRawLinesHandlerDelegate;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.text.CharArrayUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

/**
 * Joins two adjacent Erlang line comments, removing the whole leading run of '%' of the second one
 * (and only that run, so '%' characters of the comment text are kept). Comments wrap at the right margin
 * the same way the platform's CommentJoinLinesHandler wraps them.
 */
public class ErlangCommentJoinLinesHandler implements JoinRawLinesHandlerDelegate {
  @Override
  public int tryJoinRawLines(@NotNull Document document, @NotNull PsiFile file, int start, int end) {
    if (!(file instanceof ErlangFile)) return CANNOT_JOIN;
    CharSequence text = document.getCharsSequence();
    if (end >= text.length() || text.charAt(end) != '%') return CANNOT_JOIN;
    PsiComment prevComment = PsiTreeUtil.getNonStrictParentOfType(file.findElementAt(start - 1), PsiComment.class);
    PsiComment nextComment = PsiTreeUtil.getNonStrictParentOfType(file.findElementAt(end), PsiComment.class);
    if (prevComment == null || nextComment == null || prevComment == nextComment) return CANNOT_JOIN;

    int lineNumber = document.getLineNumber(start);
    int nextEnd = document.getLineEndOffset(lineNumber + 1);
    int textStart = CharArrayUtil.shiftForward(text, CharArrayUtil.shiftForward(text, end, nextEnd, "%"), nextEnd, " \t");
    if (textStart == nextEnd) return CANNOT_JOIN;

    int lineStart = document.getLineStartOffset(lineNumber);
    int margin = CodeStyle.getSettings(file).getRightMargin(file.getLanguage());
    int lineLength = start - lineStart;
    if (lineLength <= margin && lineLength + (nextEnd - textStart) + 1 > margin) {
      int allowedEnd = textStart + margin - lineLength - 1;
      while (allowedEnd > textStart && !Character.isWhitespace(text.charAt(allowedEnd))) {
        allowedEnd--;
      }
      if (allowedEnd <= textStart) return textStart;
      int endOfMovedPart = CharArrayUtil.shiftBackward(text, allowedEnd, " \t") + 1;
      CharSequence toMove = text.subSequence(textStart, endOfMovedPart).toString();
      int lineBreakPos = document.getLineEndOffset(lineNumber);
      document.deleteString(textStart, allowedEnd + 1);
      document.replaceString(start, lineBreakPos, " " + toMove);
      return start + 1 + toMove.length() + (textStart - lineBreakPos);
    }

    document.replaceString(start, textStart, " ");
    return start;
  }

  @Override
  public int tryJoinLines(@NotNull Document document, @NotNull PsiFile file, int start, int end) {
    return CANNOT_JOIN;
  }
}
