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

package org.intellij.erlang.highlighting;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.highlighter.EditorHighlighter;
import com.intellij.openapi.editor.highlighter.EditorHighlighterFactory;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangEditorHighlighterTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // #808: typing \ before the closing quote of the last string, then n, must leave "hello\n" one string token.
  public void testEscapeBeforeClosingQuoteOfLastString() {
    myFixture.configureByText("a.erl", "f() -> \"hello<caret>\".");
    myFixture.type("\\n");
    assertEquals("f() -> \"hello\\n\".", myFixture.getEditor().getDocument().getText());
    checkIncrementalHighlighting();
  }

  // #808: deleting the closing quote of the last string and typing it back must make "hello world" one string token again.
  public void testRetypeClosingQuoteOfLastString() {
    myFixture.configureByText("a.erl", "f() -> \"hello world\".");
    Document document = myFixture.getEditor().getDocument();
    int quote = document.getText().lastIndexOf('"');
    WriteCommandAction.runWriteCommandAction(getProject(), () -> document.deleteString(quote, quote + 1));
    WriteCommandAction.runWriteCommandAction(getProject(), () -> document.insertString(quote, "\""));
    assertEquals("f() -> \"hello world\".", myFixture.getEditor().getDocument().getText());
    checkIncrementalHighlighting();
  }

  private void checkIncrementalHighlighting() {
    EditorEx editor = (EditorEx) myFixture.getEditor();
    EditorHighlighter fresh = EditorHighlighterFactory.getInstance()
      .createEditorHighlighter(getProject(), myFixture.getFile().getVirtualFile());
    fresh.setText(editor.getDocument().getImmutableCharSequence());
    assertEquals(dump(fresh, editor), dump(editor.getHighlighter(), editor));
  }

  private static String dump(EditorHighlighter highlighter, EditorEx editor) {
    CharSequence text = editor.getDocument().getImmutableCharSequence();
    StringBuilder sb = new StringBuilder();
    for (HighlighterIterator it = highlighter.createIterator(0); !it.atEnd(); it.advance()) {
      sb.append(it.getTokenType()).append(" ('")
        .append(text, it.getStart(), it.getEnd()).append("')\n");
    }
    return sb.toString();
  }
}
