package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Q2: an include that does not resolve (a dependency outside the project) no longer turns the layer off for the whole
 * file. The layer works with the macros it can see; a form that uses a macro the file cannot see gets master's
 * behaviour (that macro may come from the missing header).
 */
public class ErlangMacroLayerMissingIncludeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  private List<String> syntaxErrors(PsiFile f) {
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> errors = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() == HighlightSeverity.ERROR) errors.add(f.getText().substring(h.getStartOffset(), h.getEndOffset()) + ": " + h.getDescription());
    }
    return errors;
  }

  private static String target(PsiFile f, String marker) throws Exception {
    int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "none" : t.getText().split("\n")[0];
    });
  }

  public void testTheLayerWorksWithWhatItCanSee() throws Exception {
    PsiFile f = myFixture.addFileToProject("mi/a.erl", """
      -module(a).
      -include_lib("missing_dep/include/missing.hrl").
      -export([main/0]).
      -define(PAR, ()).
      main() ->
        foo?PAR,
        ok.
      foo() -> ok.
      """);
    assertEquals("-define(PAR, ()).", target(f, "foo?P|AR"));
    assertEquals("foo() -> ok.", target(f, "f|oo?PAR"));
    assertEquals("no false syntax error at foo?PAR", List.of(), syntaxErrors(f));
    assertNull(ErlangMacroLayer.refusalForTests((ErlangFile)f));
  }

  public void testAFormThatUsesAnUnseenMacroGetsMastersBehaviour() throws Exception {
    PsiFile f = myFixture.addFileToProject("mi/b.erl", """
      -module(b).
      -include("missing.hrl").
      -export([main/0, other/0]).
      -define(PAR, ()).
      main() ->
        foo?PAR,
        ?FROM_MISSING.
      other() ->
        foo?PAR.
      foo() -> ok.
      """);
    assertEquals("the form with ?FROM_MISSING: master's answer", "none", target(f, "f|oo?PAR,\n  ?FROM"));
    assertEquals("the other form: the layer's answer", "foo() -> ok.", target(f, "f|oo?PAR.\nfoo"));
  }
}
