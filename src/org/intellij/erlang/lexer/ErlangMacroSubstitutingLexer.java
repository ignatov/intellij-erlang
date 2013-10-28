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

package org.intellij.erlang.lexer;

import com.intellij.lexer.Lexer;
import com.intellij.lexer.LexerPosition;
import com.intellij.lexer.LookAheadLexer;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;

/**
 * @author savenko
 */
public class ErlangMacroSubstitutingLexer extends LookAheadLexer {
  public ErlangMacroSubstitutingLexer() {
    this(new ErlangFormsLexer());
  }

  public ErlangMacroSubstitutingLexer(Lexer baseLexer) {
    super(baseLexer);
  }

  @Override
  protected void lookAhead(Lexer baseLexer) {
    if (baseLexer.getTokenType() == ErlangInterimTokenTypes.FORM) {
      formLookAhead(baseLexer.getBufferSequence(), baseLexer.getTokenStart(), baseLexer.getTokenEnd());
      baseLexer.advance();
    } else {
      super.lookAhead(baseLexer);
    }
  }

  private void formLookAhead(CharSequence formBuffer, int formStartIdx, int formEndIdx) {
    ErlangLexer lexer = new ErlangLexer();
    lexer.start(formBuffer, formStartIdx, formEndIdx);

    if (lexer.getTokenType() == ErlangTypes.ERL_OP_MINUS) {
      addTokenAndWhitespaceFrom(lexer);
      String attributeName = lexer.getTokenText();
      addTokenAndWhitespaceFrom(lexer);
      if ("define".equals(attributeName)) {
        macroDefinitionLookAhead(lexer);
      } else if ("include".equals(attributeName)) {
        includeLookAhead(lexer);
      } else if ("include_lib".equals(attributeName)) {
        includeLibLookAhead(lexer);
      } else if ("undef".equals(attributeName)) {
        undefLookAhead(lexer);
      }
    }
    //consume remaining tokens
    addAllTokensFrom(lexer);
  }

  private void macroDefinitionLookAhead(ErlangLexer lexer) {
    if (lexer.getTokenType() != ErlangTypes.ERL_PAR_LEFT) return;
    addTokenAndWhitespaceFrom(lexer);
    ErlangMacroBuilder macroBuilder = new ErlangMacroBuilder();

    //TODO handle quoted atom_names!
    //macro name and arguments list
    if (lexer.getTokenType() != ErlangTypes.ERL_ATOM_NAME && lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
    macroBuilder.setName(lexer.getTokenText());
    addTokenAndWhitespaceFrom(lexer);
    if (lexer.getTokenType() == ErlangTypes.ERL_PAR_LEFT) {
      macroBuilder.setHasParameters(true);
      addTokenAndWhitespaceFrom(lexer);
      if (lexer.getTokenType() == ErlangTypes.ERL_PAR_RIGHT) {
        addTokenAndWhitespaceFrom(lexer);
      } else {
        if (lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
        macroBuilder.addParameter(lexer.getTokenText());
        addTokenAndWhitespaceFrom(lexer);
        while (lexer.getTokenType() != ErlangTypes.ERL_PAR_RIGHT) {
          if (lexer.getTokenType() != ErlangTypes.ERL_COMMA) return;
          addTokenAndWhitespaceFrom(lexer);
          if (lexer.getTokenType() != ErlangTypes.ERL_VAR) return;
          macroBuilder.addParameter(lexer.getTokenText());
          addTokenAndWhitespaceFrom(lexer);
        }
        addTokenAndWhitespaceFrom(lexer);
      }
    }

    if (lexer.getTokenType() != ErlangTypes.ERL_COMMA) return;
    int lastTokenEnd = lexer.getTokenEnd();
    addTokenFrom(lexer);

    //macro body
    addToken(lastTokenEnd, ErlangInterimTokenTypes.ERL_MACRO_BODY_BEGIN);
    addWhitespaceAndCommentsFrom(lexer);
    int tokensAfterLastRightParenthesis = 0; //includes the ')' token itself
    LexerPosition lexerRightParenthesisPosition = null;
    while (lexer.getTokenType() != null && lexer.getBufferEnd() != lexer.getTokenEnd()) {
      if (lexer.getTokenType() == ErlangTypes.ERL_PAR_RIGHT) {
        lexerRightParenthesisPosition = lexer.getCurrentPosition();
        tokensAfterLastRightParenthesis = 0;
      }
      if (lexerRightParenthesisPosition != null) {
        tokensAfterLastRightParenthesis++;
      }
      macroBuilder.addBodyToken(lexer.getTokenType(), getTokenText());
      addTokenFrom(lexer);
      lastTokenEnd = lexer.getTokenEnd();
    }
    if (lexer.getTokenType() == ErlangTypes.ERL_DOT && lexerRightParenthesisPosition != null) {
      lexer.restore(lexerRightParenthesisPosition);
      resetCacheSize(getCacheSize() - tokensAfterLastRightParenthesis);
      macroBuilder.dropBodyTokens(tokensAfterLastRightParenthesis);
      lastTokenEnd = lexer.getTokenStart();
    }
    addToken(lastTokenEnd, ErlangInterimTokenTypes.ERL_MACRO_BODY_END);
    addAllTokensFrom(lexer);
    ErlangMacro macro = macroBuilder.build();
    //TODO store macro
  }

  private void includeLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void includeLibLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void undefLookAhead(Lexer lexer) {
    //TODO implement
  }

  private void addAllTokensFrom(Lexer lexer) {
    //TODO substitute macros before adding tokens
    while (lexer.getTokenType() != null) {
      addTokenFrom(lexer);
    }
  }

  private void addTokenAndWhitespaceFrom(Lexer lexer) {
    addTokenFrom(lexer);
    addWhitespaceAndCommentsFrom(lexer);
  }

  private void addWhitespaceAndCommentsFrom(Lexer lexer) {
    while (ErlangParserDefinition.WS.contains(lexer.getTokenType()) ||
      ErlangParserDefinition.COMMENTS.contains(lexer.getTokenType())) {
      addTokenFrom(lexer);
    }
  }

  private void addTokenFrom(Lexer lexer) {
    if (lexer.getTokenType() != null) {
      addToken(lexer.getTokenEnd(), lexer.getTokenType());
      lexer.advance();
    }
  }
}
