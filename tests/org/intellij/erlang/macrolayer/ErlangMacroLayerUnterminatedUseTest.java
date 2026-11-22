package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * #1451: a macro use whose argument list is not closed. erlc 27 names the use
 * ({@code err_unterminated.erl:5:4: badly formed argument for macro 'ID'}), so the error must be on {@code ?ID}.
 */
public class ErlangMacroLayerUnterminatedUseTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String TEXT = """
    -module(err_unterminated).
    -compile([export_all, nowarn_export_all]).
    -define(ID(X), X).
    f() ->
      ?ID(1.
    g() ->
      ok.
    """;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  public void testErrorIsOnTheUnterminatedMacroUse() {
    myFixture.configureByText("err_unterminated.erl", TEXT);
    int use = TEXT.indexOf("?ID(1.");
    List<String> errors = new ArrayList<>();
    boolean onUse = false;
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() != HighlightSeverity.ERROR) continue;
      int line = StringUtil.offsetToLineNumber(TEXT, h.getStartOffset());
      int column = h.getStartOffset() - StringUtil.lineColToOffset(TEXT, line, 0) + 1;
      errors.add((line + 1) + ":" + column + " " + h.getDescription());
      if (h.getStartOffset() >= use && h.getStartOffset() <= use + "?ID".length()) onUse = true;
    }
    assertTrue("an error at 5:3-5:5 (?ID), as erlc reports 5:4; errors: " + errors, onUse);
  }
}
