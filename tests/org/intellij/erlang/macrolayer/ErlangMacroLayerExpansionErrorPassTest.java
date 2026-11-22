package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: what makes the separate expansion-error pass expand and parse again. Its result depends only on the text
 * of the file and the directive lines of the headers it includes (the key of the layer's copy); a rerun after any
 * other event reuses the cached copy and its cached errors. Counted with the layer's build log and its parse counter.
 */
public class ErlangMacroLayerExpansionErrorPassTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private PsiFile myMain, myHeader, myOther;

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
    ErlangMacroLayerLog.enableForTests(true);
    myHeader = myFixture.addFileToProject("p/h.hrl", "-define(HPAR, ()).\n-record(hr, {a}).\n");
    myOther = myFixture.addFileToProject("p/other.erl", "-module(other).\n-export([z/0]).\nz() -> ok.\n");
    myMain = myFixture.addFileToProject("p/main.erl", "-module(main).\n-include(\"h.hrl\").\n-export([a/0, b/0]).\n-define(PAR, ()).\n" +
                                                       "a() -> ?PAR, ok.\nb() -> ?HPAR, ok.\n");
    myFixture.configureFromExistingVirtualFile(myMain.getVirtualFile());
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

  /** Highlights main.erl (all passes, the external one too); returns the expansion errors and asserts both are there. */
  private List<String> highlightMain() {
    myFixture.configureFromExistingVirtualFile(myMain.getVirtualFile());
    List<String> r = new ArrayList<>();
    for (HighlightInfo i : myFixture.doHighlighting(HighlightSeverity.ERROR)) {
      if (String.valueOf(i.getDescription()).startsWith("The expansion of")) r.add(i.getDescription());
    }
    assertEquals(r.toString(), 2, r.size());
    return r;
  }

  /** Builds and parses of the layer for main.erl during the action and the highlighting after it. */
  private String counted(Runnable action) {
    highlightMain();
    ErlangMacroLayerLog.reset();
    int parsed0 = ErlangMacroLayer.COPIES_PARSED.get();
    action.run();
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    highlightMain();
    long builds = builds().stream().filter(e -> e.file().equals("main.erl")).count();
    return "builds=" + builds + " parsed=" + (ErlangMacroLayer.COPIES_PARSED.get() - parsed0);
  }

  private void edit(PsiFile f, String marker, String replacement) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    assertNotNull(d);
    int at = d.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.replaceString(at, at + marker.length(), replacement));
  }

  /** (a) Caret moves in the file: nothing expanded or parsed again. */
  public void testCaretMovesExpandNothing() {
    assertEquals("builds=0 parsed=0", counted(() -> {
      for (int o : new int[]{3, 40, 80, 100, 10}) myFixture.getEditor().getCaretModel().moveToOffset(o);
    }));
  }

  /** (b) An edit in an unrelated file: nothing again. */
  public void testUnrelatedEditExpandsNothing() {
    assertEquals("builds=0 parsed=0", counted(() -> edit(myOther, "z() -> ok.", "z() -> okk.")));
  }

  /** (c) An edit of a header outside its directive lines: nothing again. */
  public void testHeaderEditOutsideDirectivesExpandsNothing() {
    assertEquals("builds=0 parsed=0", counted(() -> edit(myHeader, "{a}", "{a, b}")));
  }

  /** (d) An edit of the file: one build. */
  public void testEditOfTheFileExpandsOnce() {
    assertEquals("builds=1 parsed=1", counted(() -> edit(myMain, "b() -> ?HPAR, ok.", "b() -> ?HPAR, okk.")));
  }

  /** (e) An edit of a directive line of the included header: one build. */
  public void testHeaderDirectiveEditExpandsOnce() {
    assertEquals("builds=1 parsed=1", counted(() -> edit(myHeader, "-define(HPAR, ()).", "-define(HPAR, (())).")));
  }
}
