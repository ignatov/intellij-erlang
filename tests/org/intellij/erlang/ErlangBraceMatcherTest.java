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
import com.intellij.openapi.actionSystem.IdeActions;
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

  public void testFunTypeInSpecIsNotBrace() {
    doTestFunTypeIsNotBrace("-spec f(<caret>fun((a) -> b)) -> ok.\nf(_) -> ok.");
  }

  public void testFunTypeInTypeIsNotBrace() {
    doTestFunTypeIsNotBrace("-type h() :: <caret>fun().");
  }

  public void testFunTypeInCallbackIsNotBrace() {
    doTestFunTypeIsNotBrace("-callback f() -> <caret>fun((...) -> ok).");
  }

  public void testFunTypeWithConstraintInSpecIsNotBrace() {
    doTestFunTypeIsNotBrace("-spec f(X) -> <caret>fun() when X :: integer().\nf(_) -> fun() -> ok end.");
  }

  public void testFunTypeWithConstraintInCallbackIsNotBrace() {
    doTestFunTypeIsNotBrace("-callback f(X) -> <caret>fun() when X :: atom().");
  }

  public void testNavigationIgnoresFunTypeWithConstraintInSpec() {
    doTestCaretStays(IdeActions.ACTION_EDITOR_MATCH_BRACE,
                     "-module(a).\n-spec f(X) -> fun() when X :: integer().\nf(_) -> fun() -> ok end.\ng(X) -> X +<caret> 1.\n");
    doTestCaretStays("EditorCodeBlockStart",
                     "-module(a).\n-spec f(X) -> fun() when X :: integer().\nf(_) -> fun() -> ok end.\ng(X) -> X +<caret> 1.\n");
  }

  public void testLambdaWithGuardSequenceMatchesEnd() {
    doTest(true, "f() -> <caret>fun() when a; b, c -> ok <match>end.");
  }

  public void testFunTypeInRecordFieldIsNotBrace() {
    doTestFunTypeIsNotBrace("-record(r, {f = fun() -> ok end :: <caret>fun(() -> ok)}).");
  }

  public void testLambdaInRecordFieldDefaultMatchesEnd() {
    doTest(true, "-record(r, {f = <caret>fun() -> ok <match>end :: fun(() -> ok)}).");
  }

  public void testLambdaWithGuardMatchesEnd() {
    doTest(true, "f() -> <caret>fun() when true -> fun() -> ok end <match>end.");
  }

  public void testLambdaWithParenthesizedPatternMatchesEnd() {
    doTest(true, "f() -> <caret>fun((X), {Y}) -> X(Y) <match>end.");
  }

  public void testCodeBlockStartIgnoresFunTypeInSpec() {
    doTestCaretStays("EditorCodeBlockStart");
  }

  public void testMatchBraceIgnoresFunTypeInSpec() {
    doTestCaretStays(IdeActions.ACTION_EDITOR_MATCH_BRACE);
  }

  private void doTestCaretStays(String actionId) {
    doTestCaretStays(actionId, "-module(a).\n-spec f(fun((a) -> b)) -> ok.\nf(_) -> ok.\ng(X) -> X +<caret> 1.\n");
  }

  private void doTestCaretStays(String actionId, String text) {
    myFixture.configureByText("a.erl", text);
    int offset = myFixture.getCaretOffset();
    myFixture.performEditorAction(actionId);
    assertEquals(offset, myFixture.getCaretOffset());
  }

  private void doTestFunTypeIsNotBrace(String text) {
    myFixture.configureByText("a.erl", text);
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
