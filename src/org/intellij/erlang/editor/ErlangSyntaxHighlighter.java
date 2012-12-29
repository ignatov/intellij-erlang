/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.parser.ErlangLexer;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;
import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangSyntaxHighlighter extends SyntaxHighlighterBase {
  public static final TextAttributes KEYWORD_DEFAULTS = SyntaxHighlighterColors.KEYWORD.getDefaultAttributes();
  public static final TextAttributes RECORD_DEFAULTS = CodeInsightColors.INSTANCE_FIELD_ATTRIBUTES.getDefaultAttributes();
  
  public static final TextAttributesKey ILLEGAL = createTextAttributesKey("ERL_ILLEGAL", SyntaxHighlighterColors.INVALID_STRING_ESCAPE.getDefaultAttributes());
  public static final TextAttributesKey COMMENT = createTextAttributesKey("ERL_COMMENT", SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes());
  public static final TextAttributesKey STRING = createTextAttributesKey("ERL_STRING", SyntaxHighlighterColors.STRING.getDefaultAttributes());
  public static final TextAttributesKey NUMBER = createTextAttributesKey("ERL_NUMBER", SyntaxHighlighterColors.NUMBER.getDefaultAttributes());
  public static final TextAttributesKey KEYWORD = createTextAttributesKey("ERL_KEYWORD", new TextAttributes(KEYWORD_DEFAULTS.getForegroundColor(), KEYWORD_DEFAULTS.getBackgroundColor(), null, null, Font.BOLD));
  public static final TextAttributesKey PARENTHS = createTextAttributesKey("ERL_PARENTHS", SyntaxHighlighterColors.PARENTHS.getDefaultAttributes());
  public static final TextAttributesKey BRACES = createTextAttributesKey("ERL_BRACES", SyntaxHighlighterColors.BRACES.getDefaultAttributes());
  public static final TextAttributesKey BRACKETS = createTextAttributesKey("ERL_BRACKETS", SyntaxHighlighterColors.BRACKETS.getDefaultAttributes());
  public static final TextAttributesKey VARIABLES = createTextAttributesKey("ERL_VARIABLES", new TextAttributes(RECORD_DEFAULTS.getForegroundColor(), RECORD_DEFAULTS.getBackgroundColor(), null, null, Font.PLAIN));
  public static final TextAttributesKey RECORDS = createTextAttributesKey("ERL_RECORDS", new TextAttributes(RECORD_DEFAULTS.getForegroundColor(), RECORD_DEFAULTS.getBackgroundColor(), null, null, Font.BOLD));
  public static final TextAttributesKey OP_SIGN = createTextAttributesKey("ERL_OP_SIGN", SyntaxHighlighterColors.OPERATION_SIGN.getDefaultAttributes());
  public static final TextAttributesKey DOC_COMMENT_TAG = createTextAttributesKey("ERL_BOLD", SyntaxHighlighterColors.DOC_COMMENT_TAG.getDefaultAttributes());
  public static final TextAttributesKey KNOWN_ATOM = createTextAttributesKey("ERL_KNOWN_ATOM", new TextAttributes(null, null, null, null, Font.BOLD));

  @NotNull
  @Override
  public Lexer getHighlightingLexer() {
    return new ErlangLexer();
  }

  @NotNull
  @Override
  public TextAttributesKey[] getTokenHighlights(IElementType type) {
    if (type == TokenType.BAD_CHARACTER) {
      return pack(ILLEGAL);
    }
    if (ErlangParserDefinition.COMMENTS.contains(type)) {
      return pack(COMMENT);
    }
    if (type == ERL_STRING || type == ERL_CHAR) {
      return pack(STRING);
    }
    if (type == ERL_INTEGER || type == ERL_FLOAT) {
      return pack(NUMBER);
    }
    if (type == ERL_OP_EQ) {
      return pack(OP_SIGN);
    }
    if (type == ERL_PAR_LEFT || type == ERL_PAR_RIGHT) {
      return pack(PARENTHS);
    }
    if (type == ERL_CURLY_LEFT || type == ERL_CURLY_RIGHT) {
      return pack(BRACES);
    }
    if (type == ERL_BRACKET_LEFT || type == ERL_BRACKET_RIGHT) {
      return pack(BRACKETS);
    }
    if (type == ERL_VAR) {
      return pack(VARIABLES);
    }
    if (
      ERL_AFTER == type ||
        ERL_WHEN == type ||
        ERL_BEGIN == type ||
        ERL_END == type ||
        ERL_OF == type ||
        ERL_CASE == type ||
        ERL_FUN == type ||
        ERL_QUERY == type ||
        ERL_CATCH == type ||
        ERL_IF == type ||
        ERL_RECEIVE == type ||
        ERL_TRY == type ||
        ERL_DOT == type ||
        ERL_ANDALSO == type ||
        ERL_ORELSE == type ||
        ERL_DIV == type ||
        ERL_REM == type ||
        ERL_XOR == type ||
        ERL_BXOR == type ||
        ERL_BOR == type ||
        ERL_BAND == type ||
        ERL_BNOT == type ||
        ERL_AND == type ||
        ERL_OR == type ||
        ERL_NOT == type ||
        ERL_BSL == type ||
        ERL_BSR == type
//        ||
//        ERL_ARROW == type
      ) {
      return pack(KEYWORD);
    }
    return EMPTY;
  }
}
