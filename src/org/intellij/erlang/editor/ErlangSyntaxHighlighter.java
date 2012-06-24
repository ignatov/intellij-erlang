/*
 * Copyright 2011-2011 Gregory Shrago
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
import com.intellij.openapi.editor.markup.EffectType;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.parser.ErlangLexer;
import org.jetbrains.annotations.NotNull;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;
import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangSyntaxHighlighter extends SyntaxHighlighterBase {
  public static final TextAttributesKey ILLEGAL = createTextAttributesKey("ERL_ILLEGAL", SyntaxHighlighterColors.INVALID_STRING_ESCAPE.getDefaultAttributes());
  public static final TextAttributesKey COMMENT = createTextAttributesKey("ERL_COMMENT", SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes());
  public static final TextAttributesKey STRING = createTextAttributesKey("ERL_STRING", SyntaxHighlighterColors.STRING.getDefaultAttributes());
  public static final TextAttributesKey NUMBER = createTextAttributesKey("ERL_NUMBER", SyntaxHighlighterColors.NUMBER.getDefaultAttributes());
  public static final TextAttributesKey KEYWORD = createTextAttributesKey("ERL_KEYWORD", SyntaxHighlighterColors.KEYWORD.getDefaultAttributes());
  public static final TextAttributesKey TOKEN = createTextAttributesKey("ERL_TOKEN", SyntaxHighlighterColors.STRING.getDefaultAttributes());
  public static final TextAttributesKey RULE = createTextAttributesKey("ERL_RULE", SyntaxHighlighterColors.KEYWORD.getDefaultAttributes());
  public static final TextAttributesKey ATTRIBUTE = createTextAttributesKey("ERL_ATTRIBUTE", SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes());
  public static final TextAttributesKey EXTERNAL = createTextAttributesKey("ERL_EXTERNAL", SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes());
  public static final TextAttributesKey PARENTHS = createTextAttributesKey("ERL_PARENTHS", SyntaxHighlighterColors.PARENTHS.getDefaultAttributes());
  public static final TextAttributesKey BRACES = createTextAttributesKey("ERL_BRACES", SyntaxHighlighterColors.BRACES.getDefaultAttributes());
  public static final TextAttributesKey BRACKETS = createTextAttributesKey("ERL_BRACKETS", SyntaxHighlighterColors.BRACKETS.getDefaultAttributes());
  public static final TextAttributesKey VARIABLES = createTextAttributesKey("ERL_VARIABLES", CodeInsightColors.IMPLICIT_ANONYMOUS_CLASS_PARAMETER_ATTRIBUTES.getDefaultAttributes());
  public static final TextAttributesKey ANGLES = createTextAttributesKey("ERL_ANGLES", SyntaxHighlighterColors.PARENTHS.getDefaultAttributes());
  public static final TextAttributesKey OP_SIGN = createTextAttributesKey("ERL_OP_SIGN", SyntaxHighlighterColors.OPERATION_SIGN.getDefaultAttributes());
  public static final TextAttributesKey PIN = createTextAttributesKey("ERL_PIN", new TextAttributes(null, null, SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes().getForegroundColor(), EffectType.BOLD_DOTTED_LINE, 0));

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
    if (type == ErlangParserDefinition.ERL_COMMENT) {
      return pack(COMMENT);
    }
    if (type == ERL_STRING) {
      return pack(STRING);
    }
    if (type == ERL_INTEGER) {
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
        ERL_DOT == type
//        ||
//        ERL_ARROW == type
      ) {
      return pack(KEYWORD);
    }
    return EMPTY;
  }
}
