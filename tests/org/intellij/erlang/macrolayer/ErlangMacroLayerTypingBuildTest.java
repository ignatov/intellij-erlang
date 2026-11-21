package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import java.util.List;
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

  /**
   * The target of the last Z of f/0, from a passive query at the caret (round 6: the callers that the rule refuses are
   * the positively recognised passive ones), on a pooled thread under a progress indicator that is not the daemon's.
   */
  private String resolveLastZ(PsiFile f) throws Exception {
    return inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(() -> {
      int at = f.getText().lastIndexOf("Z.");
      PsiReference ref = f.findReferenceAt(at);
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "null" : t.getTextOffset() + "";
    }));
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
    // round 6 (next), item 1: the caller waits for the copy instead of answering at once; the next key press, here
    // less than the quiet time later and not committed either, makes it give up with master's answer
    java.util.concurrent.Future<String> query = ApplicationManager.getApplication().executeOnPooledThread(() -> resolveLastZ(f));
    Thread.sleep(100);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%% the next key\n"));
    assertEquals("master's answer", "null", query.get(30, java.util.concurrent.TimeUnit.SECONDS));
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
    String target = inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(() -> {
      PsiReference ref = f.findReferenceAt(f.getText().indexOf("W + Q"));
      PsiElement t = ref == null ? null : ref.resolve();
      return String.valueOf(t);
    }));
    assertEquals("null", target);
    assertEquals("no build by the caret-driven caller", 0, builds().size());
    assertTrue(ErlangMacroLayerLog.events().stream().anyMatch(e -> e.kind().equals("SKIPPED") && e.line().contains("broken-plain")));
  }

  /**
   * Round 6, the part of G4 that round 5 moved out (G4 now types its new form at the end of the file): typing inside a
   * function that uses a binding macro, with the counts of the round-6 rule pinned. While the user types, the editor
   * and the platform's identifier highlighting at the caret (a passive query, run after every key) build nothing;
   * highlighting builds the copy once; a caller that is not passive (here a resolve on a pooled thread) builds it
   * right after a key press and gets the layer's answer, and highlighting then reuses that copy.
   */
  public void testTypingInsideAFunctionThatUsesAMacro() throws Exception {
    PsiFile f = myFixture.addFileToProject("w.erl", "-module(w).\n-define(BIND(V), V = 1).\n-export([f/0]).\nf() -> ?BIND(Z), Z.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(1, builds().size());
    String binding = String.valueOf(f.getText().indexOf("Z)"));
    com.intellij.openapi.editor.Editor editor = myFixture.getEditor();
    editor.getCaretModel().moveToOffset(f.getText().lastIndexOf("Z."));
    // round 6 (next), item 1: the identifier highlighting after each key waits for the copy; the next key, 80 ms
    // later, makes it give up (a blocking read action here, so it gives up rather than being cancelled)
    List<java.util.concurrent.Future<?>> queries = new java.util.ArrayList<>();
    for (char c : "Y = Z + 1, ".toCharArray()) {
      myFixture.type(c);
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      int caret = editor.getCaretModel().getOffset();
      queries.add(ApplicationManager.getApplication().executeOnPooledThread(() -> ProgressManager.getInstance().runProcess(() -> ReadAction.run(
        () -> new IdentifierHighlightingComputer(f, editor, ProperTextRange.create(0, f.getTextLength()), caret).computeRanges()),
                                                                                                             new EmptyProgressIndicator())));
      Thread.sleep(80);
    }
    assertEquals("f() -> ?BIND(Z), Y = Z + 1, Z.", f.getText().substring(f.getText().indexOf("f() ->")).trim());
    assertEquals("no build while typing: the editor and the identifier highlighting", 1, builds().size());
    assertTrue("the identifier highlighting was refused", count("SKIPPED") > 0);

    myFixture.doHighlighting();
    for (java.util.concurrent.Future<?> q : queries) q.get(30, java.util.concurrent.TimeUnit.SECONDS);
    assertEquals("highlighting builds once; the last identifier highlighting waited for that copy", 2, builds().size());
    assertEquals(binding, resolveInBackground(f, "Z + 1"));
    assertEquals(binding, resolveInBackground(f, "Z."));
    assertEquals("the other callers use that copy", 2, builds().size());

    myFixture.type(' ');
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals("a caller that is not passive builds right after a key press", binding, resolveInBackground(f, "Z."));
    assertEquals(3, builds().size());
    myFixture.doHighlighting();
    assertEquals("highlighting reuses that copy", 3, builds().size());
  }

  /** The target of the reference at the marker, from a pooled thread under a progress indicator (not a passive query). */
  private static String resolveInBackground(PsiFile f, String marker) throws Exception {
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(f.getText().lastIndexOf(marker));
      PsiElement t = ref == null ? null : ref.resolve();
      return t == null ? "null" : t.getTextOffset() + "";
    });
  }

  /**
   * Round 6, item 10 (the one build per IDE session of erl_types.erl): a file whose macros are all plain, with a form
   * broken while the user types an unfinished call. The copy of that form would be broken too, so the copy could answer
   * nothing: highlighting builds no copy for it either, and shows the same errors as with the layer off.
   */
  public void testHighlightingBuildsNoCopyForAFormBrokenWithPlainMacrosOnly() throws Exception {
    PsiFile f = myFixture.addFileToProject("p.erl", "-module(p).\n-define(A, a).\n-define(P(X), {X, ?A}).\n-export([f/1]).\n" +
                                                    "f(X) -> Y = ?P(X), Y.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals(0, builds().size());
    myFixture.getEditor().getCaretModel().moveToOffset(f.getText().indexOf("Y.\n"));
    myFixture.type("lists:map(fun(Q) -> Q + Q, ");
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    List<String> layer = errors();
    assertEquals("no build by highlighting for a form broken with plain macros only", 0, builds().size());
    Disposable off = Disposer.newDisposable();
    try {
      ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, off);
      com.intellij.psi.PsiManager.getInstance(getProject()).dropPsiCaches();
      com.intellij.codeInsight.daemon.DaemonCodeAnalyzer.getInstance(getProject()).restart();
      assertEquals("the errors of master", errors(), layer);
    }
    finally {
      Disposer.dispose(off);
    }
    assertFalse(layer.isEmpty());
  }

  private List<String> errors() {
    List<String> r = new java.util.ArrayList<>();
    for (com.intellij.codeInsight.daemon.impl.HighlightInfo i : myFixture.doHighlighting()) {
      if (i.getSeverity().compareTo(com.intellij.lang.annotation.HighlightSeverity.ERROR) >= 0) r.add(i.getStartOffset() + ":" + i.getDescription());
    }
    java.util.Collections.sort(r);
    return r;
  }
}
