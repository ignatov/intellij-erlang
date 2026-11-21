package org.intellij.erlang.macrolayer;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Round 6 (next), item 5: epp's own errors that erlc reports and the plugin missed (MACRO-USE-REPORT.md 5.1, 5.3). */
public class ErlangMacroLayerEppErrorsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private List<String> problems(String name, String text) throws Exception {
    PsiFile f = myFixture.addFileToProject("e/" + name, text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    return inBackgroundWithIndicator(() -> {
      List<String> r = new ArrayList<>();
      List<ErlangMacroLayerExpansionErrors.Problem> ps = ErlangMacroLayerExpansionErrors.problems((ErlangFile)f);
      if (ps != null) {
        for (ErlangMacroLayerExpansionErrors.Problem p : ps) {
          r.add((StringUtil.offsetToLineNumber(text, p.range().getStartOffset()) + 1) + ":" + p.range().substring(text) + " " + p.message());
        }
      }
      return r;
    });
  }

  /** k39: erlc "circular macro 'A'" on the use (c5.erl:5:9). */
  public void testCircularMacro() throws Exception {
    assertEquals(List.of("5:?A The expansion of ?A is not valid Erlang: circular macro 'A'"),
                 problems("c5.erl", "-module(c5).\n-export([f/0]).\n-define(A, ?B).\n-define(B, ?A).\nf() -> ?A.\n"));
  }

  /** A circular macro that is never used: erlc compiles the file. */
  public void testUnusedCircularMacroIsNoError() throws Exception {
    assertEquals(List.of(), problems("c5b.erl", "-module(c5b).\n-export([f/0]).\n-define(A, ?A + 1).\nf() -> ok.\n"));
  }
}
