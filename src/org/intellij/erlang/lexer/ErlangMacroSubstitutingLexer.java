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
      addTokenFrom(lexer);
      addWhitespaceAndCommentsFrom(lexer);
      String attributeName = lexer.getTokenText();
      addTokenFrom(lexer);
      addWhitespaceAndCommentsFrom(lexer);
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
    //TODO implement
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

  private void addWhitespaceAndCommentsFrom(Lexer lexer) {
    while (ErlangParserDefinition.WS.contains(lexer.getTokenType()) ||
      ErlangParserDefinition.COMMENTS.contains(lexer.getTokenType())) {
      addTokenFrom(lexer);
    }
  }

  private void addTokenFrom(Lexer lexer) {
    addToken(lexer.getTokenEnd(), lexer.getTokenType());
    lexer.advance();
  }
}
