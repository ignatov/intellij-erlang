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

package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlighterPassFactory;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * #1458: with the Registry key {@code erlang.macro.layer} off, the identifier highlighting with the caret on {@code foo}
 * in {@code foo?PAR} marks nothing, as on master a858056a, also when the key is flipped off with the file open.
 */
public class ErlangMacroLayerOffIdentifierHighlightingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String KEY = "erlang.macro.layer";
  private static final String SMALL =
    "-module(m).\n" +
    "-export([par/0]).\n" +
    "-define(PAR, ()).\n" +
    "par() ->\n" +
    "  foo?PAR,\n" +
    "  foo?PAR.\n" +
    "foo() -> ok.\n";
  private static final String FRAGMENTS = "testData/macroFuzz/erl-test/src/one_file/m_fragments.erl";

  public void testOffFromTheStart() {
    Registry.get(KEY).setValue(false, getTestRootDisposable());
    myFixture.configureByText("m.erl", SMALL);
    assertEquals(List.of(), linesUnderCaret(SMALL, SMALL.indexOf("foo?PAR")));
  }

  public void testOnMarksTheFunction() {
    myFixture.configureByText("m.erl", SMALL);
    assertEquals(List.of(5, 6, 7), linesUnderCaret(SMALL, SMALL.indexOf("foo?PAR")));
  }

  public void testFlippedOffWithTheFileOpen() {
    myFixture.configureByText("m.erl", SMALL);
    int caret = SMALL.indexOf("foo?PAR");
    assertEquals(List.of(5, 6, 7), linesUnderCaret(SMALL, caret));
    flipOff();
    assertEquals("the layer is off: master's answer, not the one of the layer", List.of(), linesUnderCaret(SMALL, caret));
  }

  public void testFlippedOffWithTheReportedFileOpen() throws IOException {
    String text = Files.readString(Paths.get(FRAGMENTS));
    myFixture.configureByText("m_fragments.erl", text);
    int caret = text.indexOf("  foo?PAR,") + 2;
    assertEquals(List.of(35, 36, 38, 74), linesUnderCaret(text, caret));
    flipOff();
    assertEquals("the layer is off: master's answer, not the one of the layer", List.of(), linesUnderCaret(text, caret));
  }

  private void flipOff() {
    Registry.get(KEY).setValue(false, getTestRootDisposable());
    // the listener of the key drops the answers in invokeLater
    UIUtil.dispatchAllInvocationEvents();
    assertFalse(ErlangMacroLayer.isEnabled());
  }

  private List<Integer> linesUnderCaret(String text, int caretOffset) {
    Editor editor = myFixture.getEditor();
    List<Integer> lines = new ArrayList<>();
    IdentifierHighlighterPassFactory.doWithIdentifierHighlightingEnabled(getProject(), () -> {
      editor.getCaretModel().moveToOffset(0);
      editor.getCaretModel().moveToOffset(caretOffset);
      IdentifierHighlighterPassFactory.waitForIdentifierHighlighting(editor);
      for (RangeHighlighter highlighter : editor.getMarkupModel().getAllHighlighters()) {
        HighlightInfo info = HighlightInfo.fromRangeHighlighter(highlighter);
        if (info != null && info.getSeverity() == HighlightInfoType.ELEMENT_UNDER_CARET_SEVERITY) {
          lines.add(StringUtil.offsetToLineNumber(text, highlighter.getStartOffset()) + 1);
        }
      }
    });
    Collections.sort(lines);
    return lines;
  }
}
