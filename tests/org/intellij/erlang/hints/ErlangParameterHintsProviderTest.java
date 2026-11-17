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

package org.intellij.erlang.hints;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.hints.InlayDumpUtil;
import com.intellij.codeInsight.hints.declarative.DeclarativeInlayHintsSettings;
import com.intellij.codeInsight.hints.declarative.InlayHintsProviderFactory;
import com.intellij.codeInsight.hints.declarative.InlayProviderInfo;
import com.intellij.codeInsight.hints.declarative.impl.DeclarativeHintsPreviewProvider;
import com.intellij.codeInsight.hints.declarative.impl.DeclarativeInlayHintsPassFactory;
import com.intellij.testFramework.utils.inlays.declarative.DeclarativeInlayHintsProviderTestCase;
import org.intellij.erlang.ErlangLanguage;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.intellij.erlang.hints.ErlangParameterHintsProvider.PATTERN_SUMMARIES_OPTION;
import static org.intellij.erlang.hints.ErlangParameterHintsProvider.PROVIDER_ID;

public class ErlangParameterHintsProviderTest extends DeclarativeInlayHintsProviderTestCase {
  private static final Map<String, Boolean> SUMMARIES_ON = Map.of(PATTERN_SUMMARIES_OPTION, true);
  private static final Map<String, Boolean> SUMMARIES_OFF = Map.of(PATTERN_SUMMARIES_OPTION, false);

  public void testNamedParameters() {
    doTest("""
             -module(test).
             add(First, Second) -> First + Second.
             calc(Second) -> add(/*<# First: #>*/1, Second).
             """);
  }

  public void testNameFromAnyClause() {
    doTest("""
             -module(test).
             process(0, _) -> zero;
             process(Count, {ok, _Result}) -> Count;
             process(Count, Other) -> Other.
             test() -> process(/*<# Count: #>*/5, /*<# Other: #>*/{ok, 1}).
             """);
  }

  public void testAssignmentPatternGivesName() {
    doTest("""
             -module(test).
             -record(state, {x}).
             handle(Msg, #state{} = State) -> {Msg, State}.
             test(S) -> handle(/*<# Msg: #>*/ping, /*<# State: #>*/S).
             """);
  }

  public void testSpecNameForLiteralClauses() {
    doTest("""
             -module(test).
             -spec get(Id :: integer()) -> atom().
             get(1) -> one;
             get(2) -> two;
             get(3) -> three.
             test() -> get(/*<# Id: #>*/42).
             """);
  }

  public void testSpecNameBeatsOneLetterClauseName() {
    doTest("""
             -module(test).
             -spec get(Id :: integer()) -> atom().
             get(0) -> zero;
             get(N) -> N.
             test() -> get(/*<# Id: #>*/42).
             """);
  }

  public void testSpecTypeVariableName() {
    doTest("""
             -module(test).
             -spec get(Key) -> atom() when Key :: integer().
             get(1) -> one;
             get(2) -> two.
             test() -> get(/*<# Key: #>*/42).
             """);
  }

  public void testManyLiteralClausesAreTrimmed() {
    String clauses = IntStream.rangeClosed(1, 999)
      .mapToObj(i -> "get(" + i + ") -> " + i)
      .collect(Collectors.joining(";\n", "", ".\n"));
    doTest("-module(test).\n" + clauses + "test() -> get(/*<# 1|2|3|4|5|6…: #>*/42).\n");
  }

  public void testShortLiteralUnionIsShownInFull() {
    doTest("""
             -module(test).
             debug(true) -> on;
             debug(false) -> off.
             test() -> debug(/*<# true|false: #>*/maybe).
             """);
  }

  public void testLongSingleVariantIsTrimmed() {
    doTest("""
             -module(test).
             f(some_very_long_atom_name) -> ok.
             test() -> f(/*<# some_very_l…: #>*/x).
             """);
  }

  public void testRecordPattern() {
    doTest("""
             -module(test).
             -record(very_long_record_name, {a, b, c}).
             handle(#very_long_record_name{a = 1, b = 2, c = 3}) -> one;
             handle(#very_long_record_name{a = 2}) -> two.
             test(R) -> handle(/*<# #very_long_record_name{}: #>*/R).
             """);
  }

  public void testMapPattern() {
    doTest("""
             -module(test).
             config(#{host := H, port := P, options := O}) -> {H, P, O}.
             test(M) -> config(/*<# #{…}: #>*/M).
             """);
  }

  public void testLargeTuplePattern() {
    doTest("""
             -module(test).
             area({rectangle, Width, Height}) -> Width * Height.
             test(S) -> area(/*<# {…}: #>*/S).
             """);
  }

  public void testWildcardsGiveNoHint() {
    doTest("""
             -module(test).
             ignore(_, _Other) -> ok.
             test() -> ignore(1, /*<# Other: #>*/2).
             """);
  }

  public void testArgumentWithSameNameIsSkipped() {
    doTest("""
             -module(test).
             connect(Host, Port_Number) -> {Host, Port_Number}.
             test(Host, PortNumber) -> connect(Host, PortNumber).
             """);
  }

  public void testOneCharNamesAreSkipped() {
    doTest("""
             -module(test).
             sum(X, N1, Acc) -> X + N1 + Acc.
             test() -> sum(1, 2, /*<# Acc: #>*/3).
             """);
  }

  public void testFunctionNameContainingParameterNameIsSkipped() {
    doTest("""
             -module(test).
             set_timeout(Timeout) -> Timeout.
             test() -> set_timeout(1000).
             """);
  }

  public void testArgumentEqualToPatternIsSkipped() {
    doTest("""
             -module(test).
             mode(fast) -> 1.
             test() -> mode(fast).
             """);
  }

  public void testPatternSummariesOptionDisabled() {
    doTestProvider("test.erl", """
      -module(test).
      debug(true, Level) -> Level;
      debug(false, Level) -> Level.
      test() -> debug(maybe, /*<# Level: #>*/1).
      """, new ErlangParameterHintsProvider(), SUMMARIES_OFF);
  }

  public void testRemoteCall() {
    myFixture.addFileToProject("other.erl", """
      -module(other).
      -export([send/2]).
      send(To, Message) -> {To, Message}.
      """);
    doTest("""
             -module(test).
             test() -> other:send(/*<# To: #>*/self(), /*<# Message: #>*/hello).
             """);
  }

  public void testUnresolvedCallHasNoHints() {
    doTest("""
             -module(test).
             test() -> unknown(1, 2).
             """);
  }

  public void testPreview() {
    // the preview shown in Settings | Editor | Inlay Hints; its markers must match what the provider computes
    ErlangParameterHintsProvider provider = new ErlangParameterHintsProvider();
    String preview = DeclarativeHintsPreviewProvider.INSTANCE.getPreview(ErlangLanguage.INSTANCE, PROVIDER_ID, provider);
    assertNotNull("inlayProviders/" + PROVIDER_ID + "/preview.erl is missing", preview);
    String expected = preview.replaceAll("(?m)^\\h*/\\*<# block .*?#>\\*/\\h*\\R", "");
    assertTrue(expected.contains("/*<# "));
    doTestProvider("preview.erl", expected, provider, SUMMARIES_ON);
  }

  public void testProviderIsRegistered() {
    InlayProviderInfo info = InlayHintsProviderFactory.Companion.getProviderInfo(ErlangLanguage.INSTANCE, PROVIDER_ID);
    assertNotNull("Erlang parameter hints provider must be registered", info);
    assertInstanceOf(info.getProvider(), ErlangParameterHintsProvider.class);
    assertTrue(info.isEnabledByDefault());
    assertEquals("Parameter names", info.getProviderName());
    assertEquals(1, info.getOptions().size());
    // the old non-declarative provider must not come back: it would show hints the settings toggle cannot hide
    assertSize(1, InlayHintsProviderFactory.Companion.getProvidersForLanguage(ErlangLanguage.INSTANCE));
    assertNull(com.intellij.codeInsight.hints.InlayParameterHintsExtension.INSTANCE.forLanguage(ErlangLanguage.INSTANCE));
  }

  public void testDisabledProviderShowsNoHints() {
    String text = """
      -module(test).
      add(First, Second) -> First + Second.
      calc() -> add(1, 2).
      """;
    myFixture.configureByText("test.erl", text);
    DeclarativeInlayHintsSettings settings = DeclarativeInlayHintsSettings.Companion.getInstance();
    Boolean before = settings.isProviderEnabled(PROVIDER_ID);
    try {
      settings.setProviderEnabled(PROVIDER_ID, false);
      rehighlight();
      assertEquals(0, inlayCount());

      settings.setProviderEnabled(PROVIDER_ID, true);
      rehighlight();
      assertEquals(2, inlayCount());

      settings.setProviderEnabled(PROVIDER_ID, false);
      rehighlight();
      assertEquals(0, inlayCount());
    }
    finally {
      settings.setProviderEnabled(PROVIDER_ID, before == null || before);
    }
  }

  private void rehighlight() {
    // what the settings dialog does on apply: forget the last pass stamp and restart the daemon
    DeclarativeInlayHintsPassFactory.Companion.resetModificationStamp();
    DaemonCodeAnalyzer.getInstance(getProject()).restart(getName());
    myFixture.doHighlighting();
  }

  private int inlayCount() {
    return myFixture.getEditor().getInlayModel().getInlineElementsInRange(0, myFixture.getEditor().getDocument().getTextLength()).size();
  }

  private void doTest(String expected) {
    assertFalse(InlayDumpUtil.INSTANCE.removeInlays(expected).contains("<#"));
    doTestProvider("test.erl", expected, new ErlangParameterHintsProvider(), SUMMARIES_ON);
  }
}
