package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Round 6 (next): findings of the real IDE on the merged zip 2e587812 (items 1b and 1d of the coordinator). */
public class ErlangMacroLayerIdeFindingsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  /**
   * 1b: a passive query that only looks up the references at the caret ({@code findReferenceAt}, as the identifier
   * highlighting and ctrl-hover do for every element they visit) right after a key press: the token references exist
   * by structure and by the macro names of the file, so the lookup neither waits for the table nor is refused it.
   */
  public void testReferenceLookupRightAfterAKeyPressAsksNoTable() throws Exception {
    PsiFile f = myFixture.addFileToProject("p/main.erl", ErlangMacroUseTest.OWNER);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    int at = f.getText().indexOf("foo?PAR") + 1;
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    int walks0 = ErlangMacroLayer.STACK_WALKS.get();
    long t0 = System.nanoTime();
    String refs = inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(() -> {
      List<String> r = new ArrayList<>();
      for (int i = 0; i < 20; i++) {
        PsiReference ref = f.findReferenceAt(at);
        if (i == 0) r.add(String.valueOf(ref));
      }
      return r.toString();
    }));
    long ms = (System.nanoTime() - t0) / 1_000_000;
    System.out.println("IDE-FINDINGS 20 lookups at foo right after a key press: " + refs + " in " + ms + " ms, refusals=" + count("SKIPPED") +
                       " waits=" + count("WAITED") + " walks=" + (ErlangMacroLayer.STACK_WALKS.get() - walks0));
    assertTrue("the token reference is there: " + refs, refs.contains("ErlangMacroLayerTokenReference"));
    assertEquals("no refusal", 0, count("SKIPPED"));
    assertEquals("no wait", 0, count("WAITED"));
    assertEquals("no stack walk", 0, ErlangMacroLayer.STACK_WALKS.get() - walks0);
  }

  /**
   * 3f: the platform's external-tool pass, which ends the highlighting only after a queue delay of 300 ms, is created
   * only when an external annotator applies to the file. The expansion-error pass applies only to a file with a form
   * that needs the copy; a file whose macros are all plain (erl_types.erl) gets no such pass, as on master.
   */
  public void testExpansionErrorPassOnlyWhereAFormNeedsTheCopy() {
    PsiFile plain = myFixture.addFileToProject("p/plain3f.erl", "-module(plain3f).\n-define(A, 1).\n-export([f/1]).\nf(X) -> Y = X + ?A, Y.\ng(X) -> lists:map(fun(Q) -> W + Q, \n");
    myFixture.configureFromExistingVirtualFile(plain.getVirtualFile());
    myFixture.doHighlighting();
    assertFalse("no expansion-error pass for a file that needs no copy",
                com.intellij.lang.ExternalLanguageAnnotators.allForFile(org.intellij.erlang.ErlangLanguage.INSTANCE, plain).stream()
                  .anyMatch(a -> a instanceof ErlangMacroLayerExpansionErrorAnnotator));
    PsiFile owner = myFixture.addFileToProject("p/owner3f.erl", ErlangMacroUseTest.OWNER.replace("-module(main).", "-module(owner3f)."));
    myFixture.configureFromExistingVirtualFile(owner.getVirtualFile());
    myFixture.doHighlighting();
    assertTrue("the pass for a file with a fragment use",
               com.intellij.lang.ExternalLanguageAnnotators.allForFile(org.intellij.erlang.ErlangLanguage.INSTANCE, owner).stream()
                 .anyMatch(a -> a instanceof ErlangMacroLayerExpansionErrorAnnotator));
  }

  /** 1d: an unresolved macro used where error recovery left bare tokens is reported once per use (rebar_fetch.erl). */
  public void testUnresolvedBareMacroIsReportedOnce() {
    String text = """
      -module(r1d).
      -export([f/1]).
      -define(WITH_STACKTRACE(T, R, S), T:R:S ->).
      f(X) ->
          try g(X)
          catch
              ?WITH_STACKTRACE(error, {error, {M, R}}, S)
                  erlang:raise(error, {error, {M, R}}, S);
              throw:{no_resource, T} ->
                  throw(?PRV_ERROR({no_resource, T}));
              ?WITH_STACKTRACE(C, T, S)
                  throw(?PRV_ERROR({fetch_fail, C, T, S}))
          end.
      g(X) -> X.
      """;
    myFixture.enableInspections(new ErlangUnresolvedMacrosInspection());
    PsiFile f = myFixture.addFileToProject("p/r1d.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    List<String> problems = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getDescription() == null || !h.getDescription().startsWith("Unresolved macros")) continue;
      problems.add((StringUtil.offsetToLineNumber(text, h.getStartOffset()) + 1) + ":" + h.getDescription());
    }
    System.out.println("IDE-FINDINGS unresolved macros: " + problems);
    assertEquals(List.of("10:Unresolved macros '?PRV_ERROR'", "12:Unresolved macros '?PRV_ERROR'"), problems);
  }
}
