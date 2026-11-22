package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.inspection.ErlangUnusedVariableInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.descriptor;

/**
 * #1447: a variable or a function whose only use exists after macro expansion is not unused. Every file here
 * compiles with erlc -Wall (OTP 27) with no warning.
 */
public class ErlangMacroLayerUnusedAfterExpansionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    myFixture.enableInspections(new ErlangUnusedVariableInspection(), new ErlangUnusedFunctionInspection());
  }

  private List<String> unused(String name, String text) {
    PsiFile f = myFixture.addFileToProject("u/" + name, text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> r = new ArrayList<>();
    for (HighlightInfo i : myFixture.doHighlighting(HighlightSeverity.WEAK_WARNING)) {
      if (i.getDescription() == null || !i.getDescription().startsWith("Unused")) continue;
      int line = StringUtil.offsetToLineNumber(f.getText(), i.getStartOffset()) + 1;
      r.add(line + ":" + f.getText().substring(i.getStartOffset(), i.getEndOffset()) + " " + i.getDescription());
    }
    Collections.sort(r);
    return r;
  }

  public void testVariableUsedOnlyInMacroBody() {
    assertEquals(List.of(), unused("s1.erl", """
      -module(s1).
      -export([word_end/2]).
      -define(strip, {T1, S1} = skip(T0, S0)).
      word_end(T0, S0) -> ?strip, {T1, S1}.
      skip(T, S) -> {T, S}.
      """));
  }

  public void testFunctionCalledThroughArgumentListFragmentInMacroBody() {
    assertEquals(List.of(), unused("s2.erl", """
      -module(s2).
      -export([call/0]).
      -define(ARGS, (1, 2)).
      -define(CALL(F), F?ARGS).
      call() -> ?CALL(add).
      add(A, B) -> A + B.
      """));
  }

  public void testModulePrefixFragment() {
    assertEquals(List.of(), unused("s3.erl", """
      -module(s3).
      -export([prefix/1]).
      -define(LISTS, lists:).
      prefix(L) -> ?LISTS reverse(L).
      """));
  }
}
