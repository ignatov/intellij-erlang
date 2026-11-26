package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, last fix round (pass rule P9): a macro use that epp rejects (an empty argument, no closing ')', no definition
 * of its arity) makes erlc reject the file. The layer must not hide a syntax error of master in such a form: the form
 * is not expanded. Every module here fails in erlc 29.1 with the message in its comment; the last one compiles.
 */
public class ErlangMacroLayerRejectedUseTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    enableAll(myFixture);
  }

  private List<String> errors(PsiFile f, ErlangMacroLayer.Mode mode) {
    Disposable d = Disposer.newDisposable();
    try {
      ErlangMacroLayer.setModeForTests(mode, d);
      PsiManager.getInstance(getProject()).dropPsiCaches();
      DaemonCodeAnalyzer.getInstance(getProject()).restart();
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      List<String> r = new ArrayList<>();
      for (HighlightInfo i : myFixture.doHighlighting()) {
        if (i.getSeverity().compareTo(HighlightSeverity.ERROR) < 0 || i.getDescription() == null) continue;
        r.add((StringUtil.offsetToLineNumber(f.getText(), i.getStartOffset()) + 1) + ":" + i.getDescription());
      }
      Collections.sort(r);
      return r;
    }
    finally {
      Disposer.dispose(d);
    }
  }

  private void assertNoErrorHidden(String name, String text) {
    PsiFile f = myFixture.addFileToProject(name + ".erl", text);
    List<String> master = errors(f, ErlangMacroLayer.Mode.OFF);
    List<String> layer = errors(f, ErlangMacroLayer.Mode.LAYER);
    assertFalse(name + ": master shows a syntax error", master.isEmpty());
    // D1: an error inside the body of a well-formed -define (another form, which epp accepts) is hidden on purpose;
    // the rule here is about the form with the rejected use
    String[] lines = text.split("\n", -1);
    List<String> outsideDefines = new java.util.ArrayList<>();
    for (String e : master) {
      int line = Integer.parseInt(e.substring(0, e.indexOf(':')));
      if (!lines[line - 1].startsWith("-define(")) outsideDefines.add(e);
    }
    assertTrue(name + ": hidden by the layer: master " + master + ", layer " + layer, layer.containsAll(outsideDefines));
  }

  /** The verifier's h04 (from erl_scan.erl): "badly formed argument for macro 'STR'". */
  public void testEmptyFirstArgumentInAMultiLineBody() {
    assertNoErrorHidden("h04", """
      -module(h04).
      -export([f/2]).
      -record(st, {has_fun = false, text_fun}).
      -define(STR(Cl, St, Tmp, S),
              case ((St)#st.has_fun) of
                  true ->
                      Tmp = begin S end,
                      case ((St)#st.text_fun)((Cl), Tmp) of
                          true ->  Tmp;
                          false -> []
                      end;
                  false -> []
              end).
      f(St, Str) ->
          Anno = {?STR(, char, St, "$\\\\"++Str)},
          Anno.
      """);
  }

  /** "badly formed argument for macro 'P'". */
  public void testEmptyFirstArgument() {
    assertNoErrorHidden("h01", "-module(h01).\n-export([f/1]).\n-define(P(A, B), {A, B}).\nf(X) -> ?P(, X).\n");
  }

  /** "argument mismatch for macro 'T'". */
  public void testEmptyMiddleArgument() {
    assertNoErrorHidden("c3", "-module(c3).\n-export([f/1]).\n-define(T(A, B, C), {A, B, C}).\nf(X) -> ?T(X, , X).\n");
  }

  /** "argument mismatch for macro 'PAIR'": the other use of the form would make the copy parse. */
  public void testNoDefinitionOfTheArity() {
    assertNoErrorHidden("c5", "-module(c5).\n-export([f/0]).\n-define(OPEN, begin).\n-define(PAIR(A, B), {A, B}).\n" +
                              "f() -> ?OPEN ?PAIR(1) end.\n");
  }

  /** "badly formed argument for macro 'PAIR'": the call has no ')'. */
  public void testUnterminatedCall() {
    assertNoErrorHidden("c6", "-module(c6).\n-export([f/0]).\n-define(OPEN, begin).\n-define(PAIR(A, B), {A, B}).\n" +
                              "f() -> ?OPEN ?PAIR(1, 2 end.\n");
  }

  /** "badly formed argument for macro 'PAIR'", next to a use that makes the copy parse. */
  public void testEmptyArgumentNextToAnotherUse() {
    assertNoErrorHidden("c7", "-module(c7).\n-export([f/0]).\n-define(OPEN, begin).\n-define(PAIR(A, B), {A, B}).\n" +
                              "f() -> ?OPEN ?PAIR(, 2) end.\n");
  }

  /** Round 6, the verifier's h02: "badly formed argument for macro 'P'", with definitions of two arities. */
  public void testEmptyFirstArgumentWithTwoArities() {
    assertNoErrorHidden("h02", "-module(h02).\n-export([f/1]).\n-define(P(A, B), {A, B}).\n-define(P(A, B, C), {A, B, C}).\n" +
                               "f(X) -> ?P(, a, X).\n");
  }

  /** Round 6, the verifier's h03: "badly formed argument for macro 'P'", the last argument empty. */
  public void testEmptyLastArgument() {
    assertNoErrorHidden("h03", "-module(h03).\n-export([f/1]).\n-define(P(A, B), {A, B}).\nf(X) -> ?P(X, ).\n");
  }

  /** Round 6, the verifier's h05: "badly formed argument for macro 'W'", a body that puts the argument in brackets. */
  public void testEmptyArgumentInBrackets() {
    assertNoErrorHidden("h05", "-module(h05).\n-export([f/1]).\n-define(W(Cl, S), ((Cl), S)).\nf(X) -> ?W(, X).\n");
  }

  /** The control: erlc compiles it, and the layer still hides master's error. */
  public void testValidUsesStillExplainTheError() {
    PsiFile f = myFixture.addFileToProject("c8.erl", "-module(c8).\n-export([f/0]).\n-define(OPEN, begin).\n" +
                                                     "-define(PAIR(A, B), {A, B}).\nf() -> ?OPEN ?PAIR(1, 2) end.\n");
    // line 3, the body of OPEN, is an error in every mode; line 5 is the use
    assertTrue(errors(f, ErlangMacroLayer.Mode.OFF).stream().anyMatch(e -> e.startsWith("5:")));
    assertEquals(List.of(), errors(f, ErlangMacroLayer.Mode.LAYER).stream().filter(e -> e.startsWith("5:")).toList());
  }
}
