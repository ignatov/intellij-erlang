package org.intellij.erlang.macrolayer;

import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;

/**
 * Round 6: a macro use whose expansion erlc rejects is an error on the use. The expected errors are erlc's (OTP 29.1):
 * each file marked here with an error fails with "syntax error before: X" on that line; each file without an error
 * compiles.
 */
public class ErlangMacroLayerExpansionErrorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    // the fixture runs the external annotator on the UI thread
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
  }

  private void doTest(String text) {
    myFixture.configureByText("main.erl", text);
    myFixture.checkHighlighting(false, false, false);
  }

  private static String err(String macro, String before, String use) {
    return "<error descr=\"The expansion of ?" + macro + " is not valid Erlang: syntax error before: " + before + "\">" + use + "</error>";
  }

  /** The owner's file. erlc: main.erl:8:4: syntax error before: ')'. */
  public void testOwnerFile() {
    doTest("""
             -module(main).
             -author("ignatov").

             %% API
             -export([main/0]).

             -define(PAR, ()).

             main() ->
               foo(),
               %s,

               io:format("hello").

             foo() ->
               ok.
             """.formatted(err("PAR", "')'", "?PAR")));
  }

  /** The owner's first file compiles: no error. */
  public void testValidFragmentUseHasNoError() {
    doTest(ErlangMacroUseTest.OWNER);
  }

  /** The same macro is fine in one function and broken in another: the error is on the broken use only. */
  public void testSameMacroValidElsewhere() {
    doTest("-module(main).\n-export([a/0, b/0]).\n-define(PAR, ()).\na() -> foo?PAR.\nb() -> " + err("PAR", "')'", "?PAR") +
           ", ok.\nfoo() -> ok.\n");
  }

  /**
   * One form, two uses of the same macro: one valid, one broken. The valid one is a fragment, a syntax error of the main
   * tree; the expansion error replaces it.
   */
  public void testTwoUsesInOneFormOneBroken() {
    doTest("-module(main).\n-export([a/0]).\n-define(PAR, ()).\na() -> foo?PAR, " + err("PAR", "')'", "?PAR") + ", ok.\nfoo() -> ok.\n");
  }

  /** A plain macro and a broken one in the same form. */
  public void testPlainAndBrokenInOneForm() {
    doTest("-module(main).\n-export([a/0]).\n-define(N, 42).\n-define(PAR, ()).\na() -> X = ?N, " + err("PAR", "')'", "?PAR") + ", X.\n");
  }

  /** A macro of a header: the error is on the use in the file, the header has none. */
  public void testHeaderMacro() {
    myFixture.addFileToProject("h.hrl", "-define(HPAR, ()).\n");
    doTest("-module(main).\n-include(\"h.hrl\").\n-export([a/0]).\na() -> " + err("HPAR", "')'", "?HPAR") + ", ok.\n");
  }

  /**
   * The broken token comes from an argument: the error is on that token. erlc: syntax error before: 1. (With the plain
   * body {@code fun N/0} alone the use would need no copy, and the layer would not see it: see the report.)
   */
  public void testBrokenByArgument() {
    doTest("-module(main).\n-export([a/0]).\n-define(F(N), ok, fun N/0).\na() -> ?F(" + err("F", "1", "1") + ").\n");
  }

  /** Two uses of a macro with arguments in one form: the arguments make one valid and one broken. */
  public void testTwoUsesOfAnArgumentMacro() {
    doTest("-module(main).\n-export([a/0]).\n-define(F(N), ok, fun N/0).\na() -> ?F(a), ?F(" + err("F", "1", "1") + ").\n");
  }

  /**
   * A plain macro (its body alone is an expression) with an argument that breaks it: the main highlighting never sends
   * such a form to the copy; the separate pass checks it when erlang.macro.layer.red.plain is on (off by default).
   * erlc: syntax error before: 1.
   */
  public void testPlainMacroBrokenByArgument() {
    ErlangMacroLayerExpansionErrors.setChecksPlainFormsForTests(true, getTestRootDisposable());
    doTest("-module(main).\n-export([a/0]).\n-define(F(N), fun N/0).\na() -> ?F(" + err("F", "1", "1") + ").\n");
  }

  /** The same with the check of plain forms off (the default): nothing. */
  public void testPlainMacroBrokenByArgumentWithPlainCheckOff() {
    ErlangMacroLayerExpansionErrors.setChecksPlainFormsForTests(false, getTestRootDisposable());
    doTest("-module(main).\n-export([a/0]).\n-define(F(N), fun N/0).\na() -> ?F(1).\n");
  }

  /**
   * Not shown, on purpose: the body {@code #r.N, N} is raw tokens in the main tree (the parser cannot read
   * {@code #r.N}), and a parse error of a raw body's expansion may be the parser's own gap (see
   * ErlangMacroLayerExpansionErrors#bodiesParsed). erlc: syntax error before: 1.
   */
  public void testRawBodyIsNotReported() {
    doTest("-module(main).\n-export([a/1]).\n-record(r, {f}).\n-define(PAIRF(N), #r.N, N).\na(X) -> {X, ?PAIRF(1)}.\n");
  }

  /** The inner macro of a nested use is the broken part: the error is on the outermost use in the file. */
  public void testNestedMacro() {
    doTest("-module(main).\n-export([a/0]).\n-define(INNER, ()).\n-define(OUTER, ?INNER).\na() -> " + err("OUTER", "')'", "?OUTER") +
           ", ok.\n");
  }

  /** The user is typing: the form has a syntax error of its own, so the layer adds nothing. */
  public void testBrokenFormGetsNoMoreErrors() {
    myFixture.configureByText("main.erl", "-module(main).\n-export([a/0]).\n-define(PAR, ()).\na() -> ?PAR, X = , ok.\n");
    for (var info : myFixture.doHighlighting()) {
      assertFalse(info.getDescription(), String.valueOf(info.getDescription()).startsWith("The expansion of"));
    }
  }

  // ------------------------------------------------------------------------------------------------ epp's own errors

  /** erlc: undefined macro 'NOPE', also in a fragment position (the syntax error stays: nothing can expand the form). */
  public void testUndefinedMacroInFragmentPosition() {
    myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection());
    doTest("-module(main).\n-export([a/0]).\na() -> foo<error descr=\"Unresolved macros '?NOPE'\">?NOPE</error><error descr=\"'.' unexpected\">.</error>\nfoo() -> ok.\n");
  }

  /** erlc: argument mismatch for macro 'ID'. */
  public void testArgumentMismatch() {
    myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection());
    doTest("-module(main).\n-export([a/0]).\n-define(ID(X), X).\na() -> <error descr=\"Argument mismatch for macro '?ID': no definition takes 2 arguments\">?ID</error>(1, 2).\n");
  }

  /** A definition without parameters takes any argument list (epp leaves it as a call): no mismatch. */
  public void testNoMismatchForADefinitionWithoutParameters() {
    myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection());
    doTest("-module(main).\n-export([a/0]).\n-define(F, fun(X) -> X end).\na() -> ?F(1).\n");
  }
}
