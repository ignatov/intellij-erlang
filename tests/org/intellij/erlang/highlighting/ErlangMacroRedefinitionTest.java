package org.intellij.erlang.highlighting;

import org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/**
 * D4: epp's "redefining macro 'S'" (the owner's sample errors/err_redefine.erl). Shown only where it is certain: two
 * definitions of the same name and arity in the file, both outside any conditional, with no {@code -undef} of the name
 * and no include between them. erlc 29.1 gives each expectation below.
 */
public class ErlangMacroRedefinitionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnresolvedMacrosInspection.class);
  }

  private void doTest(String text) {
    myFixture.configureByText("r.erl", text);
    myFixture.checkHighlighting(false, false, false);
  }

  public void testOwnerSample() {
    doTest("-module(r).\n-compile([export_all, nowarn_export_all]).\n-define(SIZE, 1).\n" +
           "-define(<error descr=\"Redefining macro 'SIZE'\">SIZE</error>, 2).\nf() -> ?SIZE.\n");
  }

  public void testSameBody() {
    doTest("-module(r).\n-export([f/0]).\n-define(S, 1).\n-define(<error descr=\"Redefining macro 'S'\">S</error>, 1).\nf() -> ?S.\n");
  }

  public void testSameArityWithParameters() {
    doTest("-module(r).\n-export([f/0]).\n-define(S(X), X).\n-define(<error descr=\"Redefining macro 'S'\">S</error>(Y), Y).\nf() -> ?S(1).\n");
  }

  public void testOtherArities() {
    doTest("-module(r).\n-export([f/0]).\n-define(S, 1).\n-define(S(), 1).\n-define(S(X), X).\nf() -> {?S, ?S(), ?S(1)}.\n");
  }

  public void testUndefBetween() {
    doTest("-module(r).\n-export([f/0]).\n-define(S, 1).\n-undef(S).\n-define(S, 2).\nf() -> ?S.\n");
  }

  public void testConditionalBranches() {
    doTest("-module(r).\n-export([f/0]).\n-ifdef(T).\n-define(S, 1).\n-else.\n-define(S, 2).\n-endif.\nf() -> ?S.\n");
  }

  public void testInsideAConditionalNotShown() {
    // erlc: an error when T is not defined; the plugin does not decide branches here, so it says nothing
    doTest("-module(r).\n-export([f/0]).\n-define(S, 1).\n-ifndef(T).\n-define(S, 2).\n-endif.\nf() -> ?S.\n");
  }

  public void testIncludeBetweenNotShown() {
    myFixture.addFileToProject("u.hrl", "-undef(S).\n");
    doTest("-module(r).\n-export([f/0]).\n-define(S, 1).\n-include(\"u.hrl\").\n-define(S, 2).\nf() -> ?S.\n");
  }
}
