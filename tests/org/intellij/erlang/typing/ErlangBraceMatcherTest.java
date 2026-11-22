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

package org.intellij.erlang.typing;

import com.intellij.codeInsight.highlighting.BraceMatchingUtil;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangBraceMatcherTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testIfAttributeIsNotBrace() {
    assertFalse(isLBrace("-<caret>if(true).\nf() -> case a of a -> ok end.\n-endif."));
  }

  public void testIfAttributeAfterFormIsNotBrace() {
    assertFalse(isLBrace("-module(m).\n% comment\n- <caret>if(true).\n-endif."));
  }

  public void testEndIsNotMatchedToIfAttribute() {
    assertFalse(matchBrace("-if(true).\nf() -> ok.\n-endif.\ng() -> a <caret>end.", false));
  }

  public void testIfExpressionIsBrace() {
    assertTrue(matchBrace("f() -> <caret>if true -> ok end.", true));
  }

  public void testNegatedIfExpressionIsBrace() {
    assertTrue(matchBrace("f() -> - <caret>if true -> 1 end.", true));
  }

  private boolean isLBrace(String text) {
    HighlighterIterator iterator = configure(text);
    return BraceMatchingUtil.isLBraceToken(iterator, myFixture.getEditor().getDocument().getCharsSequence(), myFixture.getFile().getFileType());
  }

  private boolean matchBrace(String text, boolean forward) {
    HighlighterIterator iterator = configure(text);
    return BraceMatchingUtil.matchBrace(myFixture.getEditor().getDocument().getCharsSequence(), myFixture.getFile().getFileType(), iterator, forward);
  }

  private HighlighterIterator configure(String text) {
    myFixture.configureByText("a.erl", text);
    int offset = myFixture.getCaretOffset();
    return ((EditorEx) myFixture.getEditor()).getHighlighter().createIterator(offset);
  }
}
