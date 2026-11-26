package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.testFramework.LightProjectDescriptor;
import java.lang.ref.Reference;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** The copy builds of {@link ErlangMacroLayerCopies}: the waits for a build and the passive wait while a file is typed in. */
public class ErlangMacroLayerCopiesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String BINDING = "-module(t).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Z), Z.\n";

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

  public void testAwaitCancellablyGivesTheResultOfTheBuild() throws Exception {
    assertEquals("copy", onPooledThread(() -> ErlangMacroLayerCopies.awaitCancellably(CompletableFuture.completedFuture("copy"))));
  }

  public void testAwaitCancellablyGivesNullForABuildThatFailed() throws Exception {
    assertEquals("null", onPooledThread(
      () -> String.valueOf((Object)ErlangMacroLayerCopies.awaitCancellably(CompletableFuture.failedFuture(new IllegalStateException("failed"))))));
  }

  public void testAwaitCancellablyIsCancelledByAnInterrupt() throws Exception {
    assertEquals("cancelled by InterruptedException, still interrupted", onPooledThread(() -> {
      Thread.currentThread().interrupt();
      try {
        return "returned " + ErlangMacroLayerCopies.awaitCancellably(new CompletableFuture<String>());
      }
      catch (ProcessCanceledException e) {
        String cause = e.getCause() == null ? null : e.getCause().getClass().getSimpleName();
        return "cancelled by " + cause + (Thread.interrupted() ? ", still interrupted" : ", not interrupted");
      }
      finally {
        Thread.interrupted();
      }
    }));
  }

  public void testPassiveWaitIsCancelledByAnInterrupt() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(600_000, getTestRootDisposable());
    ErlangMacroLayerCopies.setPassiveWaitMillisForTests(600_000, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("t.erl", BINDING);
    Document d = edit(f, "%% typed\n");
    assertTrue(inBackgroundWithIndicator(() -> ErlangMacroLayerCopies.typedWithinQuietTime(f)));
    String outcome = inBackgroundWithIndicator(() -> {
      Thread.currentThread().interrupt();
      try {
        return "returned " + ErlangMacroLayerCopies.awaitWhileTyped(f, () -> false, "interrupt-test");
      }
      catch (ProcessCanceledException e) {
        return "cancelled" + (Thread.interrupted() ? ", still interrupted" : ", not interrupted");
      }
      finally {
        Thread.interrupted();
      }
    });
    assertEquals("cancelled, still interrupted", outcome);
    assertEquals(List.of("cancelled"), reasons("WAITED", "interrupt-test"));
    Reference.reachabilityFence(d); // the document keeps the time of its last change
  }

  /**
   * A passive query in a file just typed in, whose table is there and whose copy is not, waits; when the quiet time is
   * over it asks again and builds the copy itself.
   */
  public void testPassiveQueryWaitsAndBuildsWhenTheQuietTimeIsOver() throws Exception {
    Disposable quiet = Disposer.newDisposable();
    Disposer.register(getTestRootDisposable(), quiet);
    ErlangMacroLayer.setQuietMillisForTests(600_000, quiet);
    ErlangMacroLayerCopies.setPassiveWaitMillisForTests(600_000, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("t.erl", BINDING);
    Document d = edit(f, "%% typed\n");
    assertTableWithoutCopy(f, false);
    int waits = ErlangMacroLayerCopies.PASSIVE_WAITS.get();
    int ready = ErlangMacroLayerCopies.PASSIVE_WAITS_READY.get();
    Future<Boolean> query = ApplicationManager.getApplication().executeOnPooledThread(
      () -> inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(
        () -> ErlangMacroLayerCopies.getCopy(f, "passive-test") != null)));
    long until = System.currentTimeMillis() + 30_000;
    while (ErlangMacroLayerCopies.PASSIVE_WAITS.get() == waits && System.currentTimeMillis() < until) Thread.sleep(10);
    assertEquals("the query waits", waits + 1, ErlangMacroLayerCopies.PASSIVE_WAITS.get());
    assertFalse("the query waits while the file is typed in", query.isDone());
    assertEquals(List.of(), reasons("BUILD", "passive-test"));
    Disposer.dispose(quiet); // the user paused: the quiet time is over
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    assertTrue("the query gets the copy", query.get(30, TimeUnit.SECONDS));
    assertEquals(ready + 1, ErlangMacroLayerCopies.PASSIVE_WAITS_READY.get());
    assertEquals(List.of("ready"), reasons("WAITED", "passive-test"));
    assertEquals("the query builds the copy itself", 1, reasons("BUILD", "passive-test").size());
    Reference.reachabilityFence(d); // the document keeps the time of its last change
  }

  /** A passive query whose wait reaches its cap gets master's answer, and does not wait again under the same indicator. */
  public void testPassiveQueryGivesUpAtTheCap() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(600_000, getTestRootDisposable());
    ErlangMacroLayerCopies.setPassiveWaitMillisForTests(0, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("t.erl", BINDING);
    Document d = edit(f, "%% typed\n");
    assertTableWithoutCopy(f, false);
    int waits = ErlangMacroLayerCopies.PASSIVE_WAITS.get();
    String copies = inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(
      () -> ErlangMacroLayerCopies.getCopy(f, "cap-test") + " " + ErlangMacroLayerCopies.getCopy(f, "cap-test")));
    assertEquals("null null", copies);
    assertEquals("one wait for the query", waits + 1, ErlangMacroLayerCopies.PASSIVE_WAITS.get());
    assertEquals(List.of("cap"), reasons("WAITED", "cap-test"));
    assertEquals(List.of("typing", "typing"), reasons("SKIPPED", "cap-test"));
    assertEquals(List.of(), reasons("BUILD", "cap-test"));
    Reference.reachabilityFence(d); // the document keeps the time of its last change
  }

  /** A passive query in a file just typed in, whose only form that needs the copy is broken, does not wait. */
  public void testPassiveQueryDoesNotWaitForAFormBrokenWithPlainMacrosOnly() throws Exception {
    ErlangMacroLayer.setQuietMillisForTests(600_000, getTestRootDisposable());
    ErlangMacroLayerCopies.setPassiveWaitMillisForTests(600_000, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("v.erl", "-module(v).\n-define(A, a).\n-define(P(X), {X, ?A}).\n-export([f/1]).\n" +
                                                    "f(X) -> Y = ?P(X), Y.\n");
    ErlangMacroLayerRetention.getInstance(getProject());
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    int at = f.getText().indexOf("Y.\n");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(at, "lists:map(fun(Q) -> W + Q, "));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertTableWithoutCopy(f, true);
    int waits = ErlangMacroLayerCopies.PASSIVE_WAITS.get();
    String copy = inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(
      () -> String.valueOf(ErlangMacroLayerCopies.getCopy(f, "broken-test"))));
    assertEquals("null", copy);
    assertEquals("no wait", waits, ErlangMacroLayerCopies.PASSIVE_WAITS.get());
    assertEquals(List.of("typing"), reasons("SKIPPED", "broken-test"));
  }

  /** Valid forms (erlc compiles the module): only a use that the copy must substitute needs it. */
  public void testWhyFormsNeedTheCopy() throws Exception {
    PsiFile f = myFixture.addFileToProject("why.erl", """
      -module(why).
      -export([f/0, g/0, h/1, k/0, n/0, q/0, gen/0]).
      -define(PLAIN, 1).
      -define(TWO, 1, 2).
      -define(REC, rec).
      -define(GEN(N), N() -> ok).
      -record(rec, {a}).
      -if(?PLAIN > 0).
      -endif.
      f() -> ?PLAIN.
      g() -> [?TWO].
      h(X) -> #?REC{a = X}.
      k() -> {?MODULE, ?PLAIN}.
      n() -> ok.
      q() -> ?'PLAIN'.
      ?GEN(gen).
      """);
    assertEquals(List.of("-module(why). => null",
                         "-export([f/0, g/0, h/1, k/0, n/0, q/0, gen/0]). => null",
                         "-define(PLAIN, 1). => null",
                         "-define(TWO, 1, 2). => null",
                         "-define(REC, rec). => null",
                         "-define(GEN(N), N() -> ok). => null",
                         "-record(rec, {a}). => null",
                         "-if(?PLAIN > 0). => null",
                         "-endif. => null",
                         "f() -> ?PLAIN. => null",
                         "g() -> [?TWO]. => not-plain:TWO",
                         "h(X) -> #?REC{a = X}. => position:REC:QAtom<RecordRef<RecordExpression<ClauseBody<FunctionClause<",
                         "k() -> {?MODULE, ?PLAIN}. => null",
                         "n() -> ok. => null",
                         "q() -> ?'PLAIN'. => null",
                         "?GEN(gen). => starts-with-?"), why(f));
  }

  /** Forms broken in the main tree: a form with an error inside, and the error after a form that misses its dot. */
  public void testWhyBrokenFormsNeedTheCopy() throws Exception {
    PsiFile f = myFixture.addFileToProject("broken.erl", """
      -module(broken).
      -define(PLAIN, 1).
      m() -> ?PLAIN + .
      -type t() :: ?PLAIN
      """);
    assertEquals(List.of("-module(broken). => null",
                         "-define(PLAIN, 1). => null",
                         "m() -> ?PLAIN + . => parse-error",
                         "-type t() :: ?PLAIN => null",
                         "error: => error-form"), why(f));
  }

  /** Each top-level form of the file with the reason why it needs the copy. */
  private static List<String> why(PsiFile f) throws Exception {
    return inBackgroundWithIndicator(() -> {
      ErlangMacroLayer.Table t = ErlangMacroLayerTables.computeTable((ErlangFile)f);
      assertNull(t.refusal);
      List<String> r = new ArrayList<>();
      for (PsiElement c = f.getFirstChild(); c != null; c = c.getNextSibling()) {
        if (c instanceof PsiWhiteSpace || c instanceof PsiComment) continue;
        r.add((c instanceof PsiErrorElement ? "error:" : "") + c.getText().trim() + " => " + ErlangMacroLayerCopies.whyFormNeedsCopyForTests(t, c));
      }
      return r;
    });
  }

  /** The table of this version of the file is there (built by a caller that is not passive), its copy is not. */
  private void assertTableWithoutCopy(PsiFile f, boolean onlyBroken) throws Exception {
    String state = inBackgroundWithIndicator(() -> {
      ErlangMacroLayer.Table t = ErlangMacroLayerTables.table((ErlangFile)f);
      return t == null ? "no table" : "refusal=" + t.refusal + " copy=" + t.holder.get() + " onlyBroken=" +
                                      ErlangMacroLayerCopies.onlyBrokenFormsNeedCopy((ErlangFile)f, t);
    });
    assertEquals("refusal=null copy=null onlyBroken=" + onlyBroken, state);
  }

  private Document edit(PsiFile f, String text) {
    ErlangMacroLayerRetention.getInstance(getProject()); // its listener notes the time of the change
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    return d;
  }

  private static List<String> reasons(String kind, String trigger) {
    List<String> r = new ArrayList<>();
    for (ErlangMacroLayerLog.Event e : ErlangMacroLayerLog.events()) {
      if (e.kind().equals(kind) && e.trigger().equals(trigger)) r.add(e.reason());
    }
    return r;
  }

  private static <T> T onPooledThread(java.util.concurrent.Callable<T> c) throws Exception {
    return waitForTask(ApplicationManager.getApplication().executeOnPooledThread(c));
  }
}
