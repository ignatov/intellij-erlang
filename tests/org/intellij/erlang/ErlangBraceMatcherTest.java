/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.codeInsight.highlighting.BraceMatchingUtil;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.openapi.fileTypes.FileType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangBraceMatcherTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MATCH = "<match>";

  public void testEndMatchesLambdaNotRemoteFunReference() {
    doTest(false, "f() -> <match>fun() -> lists:map(fun m:g/1, []) <caret>end.");
  }

  public void testLambdaMatchesEndAfterRemoteFunReference() {
    doTest(true, "f() -> <caret>fun() -> lists:map(fun m:g/1, []) <match>end.");
  }

  public void testEndMatchesLambdaNotVariableFunReference() {
    doTest(false, "f(M, F) -> <match>fun(X) -> lists:map(fun M:F/1, X) <caret>end.");
  }

  public void testEndMatchesCaseNotLocalFunReference() {
    doTest(false, "f(X) -> <match>case X of [] -> fun g/1; _ -> fun 'h'/1 <caret>end.");
  }

  public void testEndMatchesNamedFun() {
    doTest(false, "f() -> <match>fun Loop(0) -> fun ?MODULE:g/1; Loop(N) -> Loop(N - 1) <caret>end.");
  }

  public void testNamedFunMatchesEnd() {
    doTest(true, "f() -> <caret>fun Loop(0) -> fun ?MODULE:g/1; Loop(N) -> Loop(N - 1) <match>end.");
  }

  public void testLambdaWithCommentBeforeParenMatchesEnd() {
    doTest(true, "f() -> <caret>fun % comment\n () -> fun g/1 <match>end.");
  }

  public void testFunReferenceIsNotBrace() {
    myFixture.configureByText("a.erl", "f() -> fun() -> lists:map(<caret>fun m:g/1, []) end.");
    HighlighterIterator iterator = createIterator();
    assertEquals(ErlangTypes.ERL_FUN, iterator.getTokenType());
    assertFalse(BraceMatchingUtil.isLBraceToken(iterator, getText(), getFileType()));
    assertEquals(myFixture.getCaretOffset(), iterator.getStart());
  }

  private void doTest(boolean forward, String textWithMarkers) {
    int match = textWithMarkers.replace("<caret>", "").indexOf(MATCH);
    myFixture.configureByText("a.erl", textWithMarkers.replace(MATCH, ""));
    HighlighterIterator iterator = createIterator();
    CharSequence text = getText();
    FileType fileType = getFileType();
    if (forward) {
      assertTrue(BraceMatchingUtil.isLBraceToken(iterator, text, fileType));
    }
    else {
      assertTrue(BraceMatchingUtil.isRBraceToken(iterator, text, fileType));
    }
    assertTrue(BraceMatchingUtil.matchBrace(text, fileType, iterator, forward));
    assertEquals(match, iterator.getStart());
  }

  private HighlighterIterator createIterator() {
    Editor editor = myFixture.getEditor();
    return editor.getHighlighter().createIterator(editor.getCaretModel().getOffset());
  }

  private CharSequence getText() {
    return myFixture.getEditor().getDocument().getCharsSequence();
  }

  private FileType getFileType() {
    return myFixture.getFile().getFileType();
  }
}
