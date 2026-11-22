package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 7: the form cap is 4,000 tokens. A big function with a small expansion (like scan_entity_value/7 of
 * xmerl_scan.erl, 2,354 tokens) is expanded and gets the layer's answers; a form over the cap is not expanded.
 */
public class ErlangMacroLayerFormCapTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private static String list(int n) {
    return IntStream.range(0, n).mapToObj(String::valueOf).collect(Collectors.joining(", ", "[", "]"));
  }

  public void testBigFunctionWithSmallExpansionIsExpanded() throws Exception {
    PsiFile f = myFixture.addFileToProject("cap.erl", "-module(cap).\n-define(B(X), X = 1).\n-export([big/0, huge/0]).\n" +
                                                      "big() -> ?B(Z), L = " + list(1200) + ", {Z, L}.\n" +
                                                      "huge() -> ?B(W), L = " + list(2200) + ", {W, L}.\n");
    String result = inBackground(() -> {
      List<String> r = new ArrayList<>();
      ErlangMacroLayer.Copy c = ErlangMacroLayer.build(f);
      for (ErlangMacroExpander.Form fm : c.expansion.forms) {
        if (fm.status != ErlangMacroExpander.FormStatus.NO_USES) r.add(fm.status + ":" + (fm.expandedTokens > 2000) + ":" + (fm.expandedTokens > 4000));
      }
      for (String marker : List.of("Z, L}", "W, L}")) {
        PsiReference ref = f.findReferenceAt(f.getText().indexOf(marker));
        PsiElement t = ref == null ? null : ref.resolve();
        r.add(marker.charAt(0) + "->" + (t == null ? "null" : "bound"));
      }
      return r.toString();
    });
    assertEquals("[EXPANDED:true:false, OVER_CAP:true:true, Z->bound, W->null]", result);
  }
}
