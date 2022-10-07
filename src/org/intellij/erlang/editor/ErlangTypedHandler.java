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

import com.intellij.codeInsight.AutoPopupController;
import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.editorActions.TypedHandlerDelegate;
import com.intellij.codeInsight.highlighting.BraceMatcher;
import com.intellij.codeInsight.highlighting.BraceMatchingUtil;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

public class ErlangTypedHandler extends TypedHandlerDelegate {
  @Override
  public @NotNull Result checkAutoPopup(char c, @NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return super.checkAutoPopup(c, project, editor, file);

    AutoPopupController autoPopupController = AutoPopupController.getInstance(project);
    if (c == ':') {
      autoPopupController.autoPopupMemberLookup(editor, null);
      return Result.STOP;
    }

    return super.checkAutoPopup(c, project, editor, file);
  }

  @Override
  public @NotNull Result charTyped(char c, @NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file) {
    if (!(file instanceof ErlangFile)) return super.charTyped(c, project, editor, file);

    if (c != '<' || !CodeInsightSettings.getInstance().AUTOINSERT_PAIR_BRACKET) {
      return Result.CONTINUE;
    }
    insertMatchedBinaryBraces(project, editor, file);
    return Result.CONTINUE;
  }

  /**
   * this is almost complete c'n'p from TypedHandler,
   * This code should be generalized into BraceMatchingUtil to support custom matching braces for plugin developers
   *
   * @see com.intellij.codeInsight.editorActions.TypedHandler
   * @see BraceMatchingUtil
   */
  private static void insertMatchedBinaryBraces(Project project, Editor editor, PsiFile file) {
    if (!(file instanceof ErlangFile)) return;

    PsiDocumentManager.getInstance(project).commitAllDocuments();

    FileType fileType = file.getFileType();
    int offset = editor.getCaretModel().getOffset();
    HighlighterIterator iterator = editor.getHighlighter().createIterator(offset);
    boolean atEndOfDocument = offset == editor.getDocument().getTextLength();

    if (!atEndOfDocument) iterator.retreat();
    if (iterator.atEnd()) return;
    BraceMatcher braceMatcher = BraceMatchingUtil.getBraceMatcher(fileType, iterator);
    if (iterator.atEnd()) return;
    IElementType braceTokenType = iterator.getTokenType();
    CharSequence fileText = editor.getDocument().getCharsSequence();
    if (!braceMatcher.isLBraceToken(iterator, fileText, fileType)) return;

    if (!iterator.atEnd()) {
      iterator.advance();

      if (!iterator.atEnd()) {
        if (!BraceMatchingUtil.isPairedBracesAllowedBeforeTypeInFileType(braceTokenType, iterator.getTokenType(), fileType)) {
          return;
        }
        if (BraceMatchingUtil.isLBraceToken(iterator, fileText, fileType)) {
          return;
        }
      }

      iterator.retreat();
    }

    int lparenOffset = BraceMatchingUtil.findLeftmostLParen(iterator, braceTokenType, fileText, fileType);
    if (lparenOffset < 0) lparenOffset = 0;

    iterator = editor.getHighlighter().createIterator(lparenOffset);

    if (!BraceMatchingUtil.matchBrace(fileText, fileType, iterator, true, true)) {
      editor.getDocument().insertString(offset, ">>");
    }
  }
}
