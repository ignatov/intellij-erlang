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

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
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
  public static final TextAttributesKey ILLEGAL       = createTextAttributesKey("ERL_ILLEGAL", DefaultLanguageHighlighterColors.INVALID_STRING_ESCAPE);
  public static final TextAttributesKey COMMENT       = createTextAttributesKey("ERL_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);
  public static final TextAttributesKey STRING        = createTextAttributesKey("ERL_STRING", DefaultLanguageHighlighterColors.STRING);
  public static final TextAttributesKey NUMBER        = createTextAttributesKey("ERL_NUMBER", DefaultLanguageHighlighterColors.NUMBER);
  public static final TextAttributesKey KEYWORD       = createTextAttributesKey("ERL_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);                      
  public static final TextAttributesKey PARENTHESES   = createTextAttributesKey("ERL_PARENTHESES", DefaultLanguageHighlighterColors.PARENTHESES);
  public static final TextAttributesKey BRACES        = createTextAttributesKey("ERL_BRACES", DefaultLanguageHighlighterColors.BRACES);
  public static final TextAttributesKey BRACKETS      = createTextAttributesKey("ERL_BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);
  public static final TextAttributesKey ATOM          = createTextAttributesKey("ERL_ATOM", DefaultLanguageHighlighterColors.IDENTIFIER);
  public static final TextAttributesKey MACRO         = createTextAttributesKey("ERL_MACRO", CodeInsightColors.STATIC_FINAL_FIELD_ATTRIBUTES);
  public static final TextAttributesKey VARIABLES     = createTextAttributesKey("ERL_VARIABLES", DefaultLanguageHighlighterColors.GLOBAL_VARIABLE);
  public static final TextAttributesKey RECORDS       = createTextAttributesKey("ERL_RECORDS", CodeInsightColors.INSTANCE_FIELD_ATTRIBUTES);
  public static final TextAttributesKey OP_SIGN       = createTextAttributesKey("ERL_OP_SIGN", DefaultLanguageHighlighterColors.OPERATION_SIGN);
  public static final TextAttributesKey DOC_TAG       = createTextAttributesKey("ERL_DOC_TAG", DefaultLanguageHighlighterColors.DOC_COMMENT_TAG);
  public static final TextAttributesKey FUNCTION      = createTextAttributesKey("ERL_FUNCTION", CodeInsightColors.METHOD_DECLARATION_ATTRIBUTES);
  public static final TextAttributesKey TYPE          = createTextAttributesKey("ERL_TYPE", CodeInsightColors.ANNOTATION_ATTRIBUTE_NAME_ATTRIBUTES);
  public static final TextAttributesKey ATTRIBUTE     = createTextAttributesKey("ERL_ATTRIBUTE", CodeInsightColors.ANNOTATION_NAME_ATTRIBUTES);
  public static final TextAttributesKey FUNCTION_CALL = createTextAttributesKey("ERL_FUNCTION_CALL", CodeInsightColors.METHOD_CALL_ATTRIBUTES);
  public static final TextAttributesKey MODULE_REF    = createTextAttributesKey("ERL_MODULE_REF", DefaultLanguageHighlighterColors.CLASS_NAME);

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
      return pack(PARENTHESES);
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
    if (type == ERL_FUNCTION) {
      return pack(FUNCTION);
    }

    if (
      ERL_AFTER == type ||
        ERL_WHEN == type ||
        ERL_BEGIN == type ||
        ERL_END == type ||
        ERL_OF == type ||
        ERL_CASE == type ||
        ERL_FUN == type ||
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
        ERL_BSR == type ||
        ERL_OR_OR == type
      ) {
      return pack(KEYWORD);
    }
    return EMPTY;
  }
}
