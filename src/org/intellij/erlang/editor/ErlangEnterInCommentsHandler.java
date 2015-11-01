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

import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegateAdapter;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.text.CharArrayUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

public class ErlangEnterInCommentsHandler extends EnterHandlerDelegateAdapter {
  @Override
  public Result preprocessEnter(@NotNull PsiFile file,
                                @NotNull Editor editor,
                                @NotNull Ref<Integer> caretOffset,
                                @NotNull Ref<Integer> caretAdvance,
                                @NotNull DataContext dataContext,
                                EditorActionHandler originalHandler) {
    if (!(file instanceof ErlangFile)) {
      return Result.Continue;
    }

    Document document = editor.getDocument();
    CharSequence text = document.getCharsSequence();
    int caret = CharArrayUtil.shiftForward(text, caretOffset.get().intValue(), " \t");
    if (caret < text.length() && text.charAt(caret) == '\n') {
      return Result.Continue;
    }

    PsiElement elementAtCaret = file.findElementAt(caret);
    ASTNode nodeAtCaret = elementAtCaret != null ? elementAtCaret.getNode() : null;
    IElementType type = nodeAtCaret != null ? nodeAtCaret.getElementType() : null;
    if (type == null ||
        !ErlangParserDefinition.COMMENTS.contains(type) ||
        ErlangParserDefinition.ERL_SHEBANG == type) {
      return Result.Continue;
    }

    TextRange commentRange = elementAtCaret.getTextRange();
    CharSequence commentText = commentRange.subSequence(text);
    int percentsCount = CharArrayUtil.shiftForward(commentText, 0, "%");
    if (commentRange.getStartOffset() + percentsCount > caret) {
      return Result.Continue;
    }

    int percentsToAppend = Math.min(3, percentsCount);
    boolean appendSpace = text.charAt(caret) != ' ';
    String newLinePrefix = StringUtil.repeat("%", percentsToAppend) + (appendSpace ? " " : "");
    document.insertString(caret, newLinePrefix);
    caretAdvance.set(newLinePrefix.length());

    return Result.Default;
  }
}
