package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 2: while a file changes, a caller that is not its highlighting (the target-element search, the
 * identifier highlighting's isReferenceTo, ...) starts no copy build: it uses the copy that is there, or gets
 * master's answer. Highlighting builds; after the quiet time any caller builds again.
 */
public class ErlangMacroLayerTypingBuildTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    // round 5: highlighting runs the Erlang inspections, as in the IDE; they ask the layer (since round 5 the name of a
    // macro call, which the parameter hints asked for, no longer does)
    enableAll(myFixture);
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

  private void edit(PsiFile f, String text) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /** The target of the last Z of f/0, from a pooled thread under a progress indicator that is not the daemon's. */
  private String resolveLastZ(PsiFile f) throws Exception {
    return inBackgroundWithIndicator(() -> {
      int at = f.getText().lastIndexOf("Z.");
      PsiReference ref = f.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "null" : t.getTextOffset() + "";
    });
  }

  public void testOtherCallersBuildNoCopyWhileTheFileChanges() throws Exception {
    Disposable quiet = Disposer.newDisposable();
    ErlangMacroLayer.setQuietMillisForTests(600_000, quiet);
    PsiFile f = myFixture.addFileToProject("t.erl", "-module(t).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n");
    String binding = String.valueOf(f.getText().indexOf("Z)"));
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(1, builds().size());
    assertEquals(binding, resolveLastZ(f));

    edit(f, "%% typed\n");
    int tables = ErlangMacroLayer.TABLES_BUILT.get();
    assertEquals("master's answer while the file changes", "null", resolveLastZ(f));
    assertEquals("no build by the other caller", 1, builds().size());
    assertEquals("no table build by the other caller either", tables, ErlangMacroLayer.TABLES_BUILT.get());
    assertEquals(1, count("SKIPPED"));

    myFixture.doHighlighting();
    assertEquals("highlighting builds", 2, builds().size());
    assertEquals("the copy of the highlighting serves the other caller", binding, resolveLastZ(f));
    assertEquals(2, builds().size());

    edit(f, "%% typed again\n");
    Disposer.dispose(quiet); // the user paused: the quiet time is over
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    assertEquals(binding, resolveLastZ(f));
    assertEquals("after the pause any caller builds", 3, builds().size());
  }

  /**
   * Round 5, last fix round: in a real editor the PSI changes only when the document is committed, and while the user
   * types into a large file the commits may fall behind the keys. The caller then reads the last committed PSI, whose
   * change is older than the quiet time, while the document changes with every key: it still starts no build.
   */
  public void testNoBuildWhileTheDocumentChangesAheadOfThePsi() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(300, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("u.erl", "-module(u).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(1, builds().size());
    edit(f, "%% typed\n");
    Thread.sleep(400); // the last commit is older than the quiet time
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%% more keys, not committed yet\n"));
    assertEquals("master's answer", "null", resolveLastZ(f));
    assertEquals("no build by the caret-driven caller", 1, builds().size());
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /**
   * Round 5, last fix round: a file whose macros are all plain, with a form broken in the middle of typing. That form
   * needs the copy only for highlighting; a caret-driven caller starts no build for it, also after the quiet time.
   */
  public void testNoBuildByOtherCallersForAFormBrokenWithPlainMacrosOnly() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("v.erl", "-module(v).\n-define(A, a).\n-define(P(X), {X, ?A}).\n-export([f/1]).\n" +
                                                    "f(X) -> Y = ?P(X), Y.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(0, builds().size());
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    int at = f.getText().indexOf("Y.\n");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(at, "lists:map(fun(Q) -> W + Q, "));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    String target = inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(f.getText().indexOf("W + Q"));
      PsiElement t = ref == null ? null : ref.resolve();
      return String.valueOf(t);
    });
    assertEquals("null", target);
    assertEquals("no build by the caret-driven caller", 0, builds().size());
    assertTrue(ErlangMacroLayerLog.events().stream().anyMatch(e -> e.kind().equals("SKIPPED") && e.line().contains("broken-plain")));
  }
}
