package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;

/**
 * The expansion error goes on the macro use that broke the form, as erlc's position says (OTP 29.1; probes named per
 * test), and nowhere when the user's own text is what is broken.
 */
public class ErlangMacroLayerExpansionBlameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
  }

  /** The problems of the file as "text of the range: message". */
  private String problems(String text) {
    PsiFile file = myFixture.configureByText("main.erl", text);
    List<ErlangMacroLayerExpansionErrors.Problem> ps = ErlangMacroLayerExpansionErrors.problems((ErlangFile)file);
    assertNotNull(ps);
    List<String> r = new ArrayList<>();
    for (ErlangMacroLayerExpansionErrors.Problem p : ps) r.add(p.range().substring(text) + ": " + p.message());
    return String.join("\n", r);
  }

  /** B9 (probe b9): the empty expansion of ?E leaves the form unfinished; erlc: syntax error before '.' after ?E. */
  public void testEmptyExpansionBeforeTheDot() {
    assertEquals("?E: The expansion of ?E is not valid Erlang: syntax error before: '.'",
                 problems("-module(b9).\n-export([u/0]).\n-define(E, ).\n-define(A, a).\nu() -> ?A, ?E.\n"));
  }

  /** B10 (probe b10b): two uses with no text between them; the broken token comes from ?B. erlc: before 2. */
  public void testAdjacentUses() {
    assertEquals("?B: The expansion of ?B is not valid Erlang: syntax error before: 2",
                 problems("-module(b10b).\n-export([f/0]).\n-define(A, 1).\n-define(B, 2, 3).\nf() -> {?A?B}.\n"));
  }

  /** D5 (probe d5): the user's own call is unclosed (erlc: syntax error before '.'); the fragment use is valid. */
  public void testUnclosedCallAfterAFragmentUse() {
    assertEquals("", problems("-module(d5).\n-export([f/0]).\n-define(PAR, ()).\nf() -> foo?PAR, g(\nok.\nfoo() -> ok.\ng(X) -> X.\n"));
  }

  /** D5: the rule still holds where the user's tokens are balanced: an argument list left open by a macro. */
  public void testUnclosedByAMacro() {
    assertEquals("?OPEN: The expansion of ?OPEN is not valid Erlang: syntax error before: '.'",
                 problems("-module(d5b).\n-export([f/0]).\n-define(OPEN, g(1).\nf() -> ?OPEN.\ng(X) -> X.\n"));
  }
}
