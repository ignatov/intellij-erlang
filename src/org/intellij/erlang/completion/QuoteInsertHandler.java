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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.AutoPopupController;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class QuoteInsertHandler extends SingleCharInsertHandler {
  private static final char QUOTE = '\'';
  private final String myName;

  protected QuoteInsertHandler(@NotNull String name) {
    super(':');
    myName = name;
  }

  @Override
  public void handleInsert(@NotNull InsertionContext context, @NotNull LookupElement item) {
    process(myName, null, context);
    if (needColon()) {
      AutoPopupController.getInstance(context.getProject()).autoPopupMemberLookup(context.getEditor(), null);
      super.handleInsert(context, item);
    }
  }

  public static void process(@NotNull String name, @Nullable String moduleName, @NotNull InsertionContext context) {
    CharSequence text = context.getDocument().getCharsSequence();
    if (moduleName != null && ErlangPsiImplUtil.needQuotation(moduleName)) {
      int startOffset = context.getStartOffset();
      int tailOffset = startOffset + moduleName.length();
      insertQuotes(context, startOffset, tailOffset, isQuoteAt(text, startOffset - 1), isQuoteAt(text, tailOffset));
    }
    int tailOffset = context.getTailOffset();
    int startOffset = moduleName == null ? context.getStartOffset() : tailOffset - name.length();
    List<Integer> quotes = getQuoteTokenOffsets(context, startOffset, tailOffset);
    int openingIndex = quotes.indexOf(startOffset - 1);
    int closingIndex = quotes.indexOf(tailOffset);
    // a quote before the name opens it unless it closes a previous atom
    boolean hasOpeningQuote = openingIndex != -1 && openingIndex % 2 == 0;
    // a quote after the name closes it unless it opens a following, closed atom
    boolean hasClosingQuote = closingIndex != -1 && (closingIndex % 2 == 1 || closingIndex == quotes.size() - 1);
    if (ErlangPsiImplUtil.needQuotation(name) || hasOpeningQuote || hasClosingQuote) {
      tailOffset = insertQuotes(context, startOffset, tailOffset, hasOpeningQuote, hasClosingQuote);
      context.getEditor().getCaretModel().moveToOffset(tailOffset);
      context.setTailOffset(tailOffset);
    }
  }

  /**
   * @return the offsets of the quote tokens of quoted atoms from the last offset before the name where the lexer is
   * outside quotes to the end of the line after the name
   */
  @NotNull
  private static List<Integer> getQuoteTokenOffsets(@NotNull InsertionContext context, int startOffset, int tailOffset) {
    Document document = context.getDocument();
    int lexStart = getLexerRestartOffset(context.getEditor(), startOffset);
    int lineEnd = document.getLineEndOffset(document.getLineNumber(tailOffset));
    List<Integer> quotes = new ArrayList<>();
    Lexer lexer = new ErlangLexer(context.getProject());
    lexer.start(document.getCharsSequence(), lexStart, lineEnd);
    for (; lexer.getTokenType() != null; lexer.advance()) {
      if (lexer.getTokenType() == ErlangTypes.ERL_SINGLE_QUOTE) {
        quotes.add(lexer.getTokenStart());
      }
    }
    return quotes;
  }

  /**
   * Only quotes and atom names can be lexed inside quotes, so the lexer is outside quotes after any other token,
   * for example after a string that spans lines.
   */
  private static int getLexerRestartOffset(@NotNull Editor editor, int startOffset) {
    if (startOffset == 0 || !(editor instanceof EditorEx)) return 0;
    HighlighterIterator iterator = ((EditorEx) editor).getHighlighter().createIterator(startOffset - 1);
    while (!iterator.atEnd()) {
      IElementType type = iterator.getTokenType();
      if (type != ErlangTypes.ERL_SINGLE_QUOTE && type != ErlangTypes.ERL_ATOM_NAME) {
        return Math.min(iterator.getEnd(), startOffset);
      }
      iterator.retreat();
    }
    return 0;
  }

  /**
   * @return the offset after the closing quote
   */
  private static int insertQuotes(@NotNull InsertionContext context, int startOffset, int tailOffset,
                                  boolean hasOpeningQuote, boolean hasClosingQuote) {
    Document document = context.getEditor().getDocument();
    if (!hasOpeningQuote) {
      document.insertString(startOffset, String.valueOf(QUOTE));
      tailOffset++;
    }
    if (!hasClosingQuote) {
      document.insertString(tailOffset, String.valueOf(QUOTE));
    }
    return tailOffset + 1;
  }

  private static boolean isQuoteAt(@NotNull CharSequence sequence, int position) {
    return position >= 0 && position < sequence.length() && sequence.charAt(position) == QUOTE;
  }

  protected boolean needColon() {
    return false;
  }

  public static class ModuleInsertHandler extends QuoteInsertHandler {
    private final boolean myWithColon;

    public ModuleInsertHandler(@NotNull String name, boolean withColon) {
      super(name);
      myWithColon = withColon;
    }

    @Override
    protected boolean needColon() {
      return myWithColon;
    }
  }
}
