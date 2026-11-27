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

import com.intellij.codeInsight.highlighting.PairedBraceMatcherAdapter;
import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangBraceMatcher extends PairedBraceMatcherAdapter {
  public static final BracePair[] PAIRS = new BracePair[]{
    new BracePair(ErlangTypes.ERL_PAR_LEFT, ErlangTypes.ERL_PAR_RIGHT, false),
    new BracePair(ErlangTypes.ERL_CURLY_LEFT, ErlangTypes.ERL_CURLY_RIGHT, false),
    new BracePair(ErlangTypes.ERL_BRACKET_LEFT, ErlangTypes.ERL_BRACKET_RIGHT, false),
    new BracePair(ErlangTypes.ERL_BIN_START, ErlangTypes.ERL_BIN_END, true),
    new BracePair(ErlangTypes.ERL_IF, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_CASE, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_BEGIN, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_TRY, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_FUN, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_RECEIVE, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_MAYBE, ErlangTypes.ERL_END, true),
  };

  public ErlangBraceMatcher() {
    super(new ErlangPairedBraceMatcher(), ErlangLanguage.INSTANCE);
  }

  @Override
  public @Nullable BracePair findPair(boolean left, @NotNull HighlighterIterator iterator, @NotNull CharSequence fileText, @NotNull FileType fileType) {
    if (left && iterator.getTokenType() == ErlangTypes.ERL_IF && isIfAttribute(iterator)) return null;
    return super.findPair(left, iterator, fileText, fileType);
  }

  // The 'if' of '-if(Expr).' starts a form, while '- if ... end' in an expression follows an operand or an operator.
  private static boolean isIfAttribute(@NotNull HighlighterIterator iterator) {
    int steps = 0;
    try {
      steps += retreatOverWhitespaceAndComments(iterator);
      if (iterator.atEnd() || iterator.getTokenType() != ErlangTypes.ERL_OP_MINUS) return false;
      steps += retreatOverWhitespaceAndComments(iterator);
      return iterator.atEnd() || iterator.getTokenType() == ErlangTypes.ERL_DOT;
    }
    finally {
      for (int i = 0; i < steps; i++) iterator.advance();
    }
  }

  private static int retreatOverWhitespaceAndComments(@NotNull HighlighterIterator iterator) {
    int steps = 0;
    do {
      iterator.retreat();
      steps++;
    }
    while (!iterator.atEnd() &&
           (iterator.getTokenType() == TokenType.WHITE_SPACE || ErlangParserDefinition.COMMENTS.contains(iterator.getTokenType())));
    return steps;
  }

  /**
   * The lexer returns ERL_FUN both for a fun block ({@code fun (...) -> ... end}, {@code fun Name(...) -> ... end})
   * and for a fun reference ({@code fun [M:]F/A}) or a fun type ({@code fun()}, {@code fun((...) -> T)}), which have no {@code end}.
   */
  @Override
  public boolean isLBraceToken(@NotNull HighlighterIterator iterator, @NotNull CharSequence fileText, @NotNull FileType fileType) {
    if (iterator.getTokenType() == ErlangTypes.ERL_FUN) return isFunBlockStart(iterator);
    return super.isLBraceToken(iterator, fileText, fileType);
  }

  private static boolean isFunBlockStart(@NotNull HighlighterIterator iterator) {
    int steps = 0;
    try {
      steps += advanceToSignificantToken(iterator);
      if (!iterator.atEnd() && iterator.getTokenType() == ErlangTypes.ERL_VAR) {
        steps += advanceToSignificantToken(iterator);
        return !iterator.atEnd() && iterator.getTokenType() == ErlangTypes.ERL_PAR_LEFT;
      }
      if (iterator.atEnd() || iterator.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return false;
      // A fun type has '->' inside its parentheses, or is 'fun()' followed by neither '->' nor a guard:
      // a 'when' after it in a spec or a callback starts constraints ('X :: T'), which have no '->'.
      int depth = 1;
      while (depth > 0) {
        steps += advanceToSignificantToken(iterator);
        if (iterator.atEnd() || iterator.getTokenType() == ErlangTypes.ERL_DOT) return true;
        IElementType type = iterator.getTokenType();
        if (type == ErlangTypes.ERL_PAR_LEFT) depth++;
        else if (type == ErlangTypes.ERL_PAR_RIGHT) depth--;
        else if (type == ErlangTypes.ERL_ARROW && depth == 1) return false;
      }
      steps += advanceToSignificantToken(iterator);
      if (iterator.atEnd() || iterator.getTokenType() == ErlangTypes.ERL_ARROW) return true;
      if (iterator.getTokenType() != ErlangTypes.ERL_WHEN) return false;
      while (true) {
        steps += advanceToSignificantToken(iterator);
        if (iterator.atEnd() || iterator.getTokenType() == ErlangTypes.ERL_ARROW) return true;
        IElementType type = iterator.getTokenType();
        if (type == ErlangTypes.ERL_DOT || type == ErlangTypes.ERL_COLON_COLON) return false;
      }
    }
    finally {
      for (int i = 0; i < steps; i++) iterator.retreat();
    }
  }

  private static int advanceToSignificantToken(@NotNull HighlighterIterator iterator) {
    int steps = 0;
    do {
      iterator.advance();
      steps++;
    }
    while (!iterator.atEnd() &&
           (iterator.getTokenType() == TokenType.WHITE_SPACE || ErlangParserDefinition.COMMENTS.contains(iterator.getTokenType())));
    return steps;
  }

  private static class ErlangPairedBraceMatcher implements PairedBraceMatcher {
    @NotNull
    @Override
    public BracePair @NotNull [] getPairs() {
      return PAIRS;
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType type) {
      return TokenType.WHITE_SPACE == type
        || ErlangParserDefinition.COMMENTS.contains(type)
        || type == ErlangTypes.ERL_SEMI
        || type == ErlangTypes.ERL_COMMA
        || type == ErlangTypes.ERL_DOT
        || type == ErlangTypes.ERL_PAR_RIGHT
        || type == ErlangTypes.ERL_BRACKET_RIGHT
        || type == ErlangTypes.ERL_CURLY_RIGHT
        || null == type;
    }

    @Override
    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
      return openingBraceOffset;
    }
  }
}
