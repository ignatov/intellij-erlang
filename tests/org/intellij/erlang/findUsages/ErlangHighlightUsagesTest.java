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

package org.intellij.erlang.findUsages;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlighterPassFactory;
import com.intellij.codeInsight.highlighting.HighlightUsagesHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.util.TextRange;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangHighlightUsagesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String VARIABLE =
    "-module(t930).\n" +
    "-export([foo/1]).\n" +
    "foo(Boo) -> Foo = Boo + 1, io:format(\"~p, ~p\", [Foo, Boo]).\n";
  private static final String CASE_BRANCHES =
    "-module(t930).\n" +
    "-export([foo/1]).\n" +
    "foo(X) -> case X of 1 -> Res = a; _ -> Res = b end, Res.\n";
  private static final String LIST_COMPREHENSION =
    "-module(t930).\n" +
    "-export([foo/1]).\n" +
    "foo(L) -> [Elem + 1 || Elem <- L, Elem > 0].\n";

  public void testVariableFromFirstOccurrence()               { doHighlightUsagesTest(VARIABLE, "Boo", 0); }
  public void testVariableFromSecondOccurrence()              { doHighlightUsagesTest(VARIABLE, "Boo", 1); }
  public void testVariableFromThirdOccurrence()               { doHighlightUsagesTest(VARIABLE, "Boo", 2); }
  public void testVariableInCaseBranchesFromLastOccurrence()  { doHighlightUsagesTest(CASE_BRANCHES, "Res", 2); }
  public void testVariableInListComprehensionFromFirstOccurrence() { doHighlightUsagesTest(LIST_COMPREHENSION, "Elem", 0); }

  public void testVariableUnderCaretFromFirstOccurrence()     { doIdentifierHighlightingTest(VARIABLE, "Boo", 0); }
  public void testVariableUnderCaretFromSecondOccurrence()    { doIdentifierHighlightingTest(VARIABLE, "Boo", 1); }
  public void testVariableUnderCaretFromThirdOccurrence()     { doIdentifierHighlightingTest(VARIABLE, "Boo", 2); }
  public void testVariableUnderCaretInCaseBranchesFromLastOccurrence() { doIdentifierHighlightingTest(CASE_BRANCHES, "Res", 2); }
  public void testVariableUnderCaretInListComprehensionFromFirstOccurrence() { doIdentifierHighlightingTest(LIST_COMPREHENSION, "Elem", 0); }

  private void doHighlightUsagesTest(String text, String name, int caretOccurrence) {
    List<TextRange> occurrences = configure(text, name, caretOccurrence);
    HighlightUsagesHandler.invoke(getProject(), myFixture.getEditor(), myFixture.getFile());
    List<TextRange> highlighted = new ArrayList<>();
    for (RangeHighlighter highlighter : myFixture.getEditor().getMarkupModel().getAllHighlighters()) {
      highlighted.add(highlighter.getTextRange());
    }
    assertSameElements(highlighted, occurrences);
  }

  private void doIdentifierHighlightingTest(String text, String name, int caretOccurrence) {
    List<TextRange> occurrences = configure(text, name, caretOccurrence);
    Editor editor = myFixture.getEditor();
    int caretOffset = editor.getCaretModel().getOffset();
    List<TextRange> highlighted = new ArrayList<>();
    IdentifierHighlighterPassFactory.doWithIdentifierHighlightingEnabled(getProject(), () -> {
      editor.getCaretModel().moveToOffset(0);
      editor.getCaretModel().moveToOffset(caretOffset);
      IdentifierHighlighterPassFactory.waitForIdentifierHighlighting(editor);
      for (RangeHighlighter highlighter : editor.getMarkupModel().getAllHighlighters()) {
        HighlightInfo info = HighlightInfo.fromRangeHighlighter(highlighter);
        if (info != null && info.getSeverity() == HighlightInfoType.ELEMENT_UNDER_CARET_SEVERITY) {
          highlighted.add(highlighter.getTextRange());
        }
      }
    });
    assertSameElements(highlighted, occurrences);
  }

  private List<TextRange> configure(String text, String name, int caretOccurrence) {
    List<TextRange> occurrences = new ArrayList<>();
    Matcher matcher = Pattern.compile("\\b" + name + "\\b").matcher(text);
    while (matcher.find()) {
      occurrences.add(new TextRange(matcher.start(), matcher.end()));
    }
    int caretOffset = occurrences.get(caretOccurrence).getStartOffset();
    myFixture.configureByText("t930.erl", text);
    myFixture.getEditor().getCaretModel().moveToOffset(caretOffset);
    return occurrences;
  }
}
