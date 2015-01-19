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

import com.intellij.lexer.LexerBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;

public final class ErlangFormsLexer extends LexerBase {
  private final _ErlangFormsLexer myLexer = new _ErlangFormsLexer();

  private CharSequence myBuffer;
  private int myBufferStartOffset;
  private int myBufferEndOffset;

  private IElementType myTokenType;
  private int myTokenStartOffset;
  private int myTokenEndOffset;

  @Override
  public void start(CharSequence buffer, int startOffset, int endOffset, int initialState) {
    myBuffer = buffer;
    myBufferStartOffset = startOffset;
    myBufferEndOffset = endOffset;
    myTokenType = null;
    myLexer.reset(buffer, myBufferStartOffset, myBufferEndOffset, initialState);
  }

  @Override
  public int getState() {
    advanceIfNoTokenType();
    return myLexer.yystate();
  }

  @Nullable
  @Override
  public IElementType getTokenType() {
    advanceIfNoTokenType();
    return myTokenType;
  }

  @Override
  public int getTokenStart() {
    advanceIfNoTokenType();
    return myTokenStartOffset;
  }

  @Override
  public int getTokenEnd() {
    advanceIfNoTokenType();
    return myTokenEndOffset;
  }

  @Override
  public void advance() {
    try {
      myTokenType = myLexer.advance();
      myTokenStartOffset = myLexer.getTokenStart();
      if (myLexer.yystate() == _ErlangFormsLexer.FORM) {
        myTokenType = ErlangInterimTokenTypes.FORM;
        while (myLexer.yystate() == _ErlangFormsLexer.FORM) {
          myLexer.advance();
        }
      }
      myTokenEndOffset = myLexer.getTokenEnd();
    } catch (IOException e) { //never happens as it doesn't read from streams
      //noinspection CallToPrintStackTrace
      e.printStackTrace();
    }
  }

  @Override
  public CharSequence getBufferSequence() {
    return myBuffer;
  }

  @Override
  public int getBufferEnd() {
    return myBufferEndOffset;
  }

  private void advanceIfNoTokenType() {
    if (myTokenType == null) {
      advance();
    }
  }
}
