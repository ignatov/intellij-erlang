package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Round 5: the defects of the macro layer found by the fourth verifier and the real-IDE check. */
public class ErlangMacroLayerDefects5Test extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
      assertEquals("a copy was built on the UI thread", List.of(), uiThreadBuilds());
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      ErlangMacroLayerLog.enableForTests(false);
      super.tearDown();
    }
  }

  static void out(String s) {
    System.out.println("LAYER-D5 " + s);
  }

  private void highlight(PsiFile... files) {
    for (PsiFile f : files) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  private static int buildsOf(String fileName) {
    return (int)builds().stream().filter(e -> e.file().equals(fileName)).count();
  }

  private static String reasons() {
    return builds().stream().map(e -> e.file() + ":" + e.reason() + ":" + e.trigger()).toList().toString();
  }

  private static PsiReference refAt(PsiFile f, String marker) {
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    return f.findReferenceAt(at);
  }

  private static String resolved(PsiFile f, String marker) throws Exception {
    return inBackground(() -> {
      PsiElement t = refAt(f, marker).resolve();
      return t == null ? "null" : t.getClass().getSimpleName() + "@" + (t.getContainingFile() == null ? "?" : t.getContainingFile().getVirtualFile().getPath());
    });
  }

  // ------------------------------------------------------------------------------------------------ defect 1

  /**
   * P5: creating, renaming or deleting a file that no include resolves to rebuilds no copy. A file event that changes
   * what an include resolves to (a header appears, disappears, or an application directory starts to shadow it)
   * rebuilds only the copies of the files whose includes change.
   */
  public void testFileEventsRebuildOnlyTheCopiesWhoseIncludesChange() throws Exception {
    myFixture.addFileToProject("d1/h.hrl", "-define(H_BIND(V), V = 42).\n");
    PsiFile a = myFixture.addFileToProject("d1/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?H_BIND(Z), Z.\n");
    PsiFile late = myFixture.addFileToProject("d1/late.erl", "-module(late).\n-include(\"late.hrl\").\n-export([f/0]).\nf() -> ?L_BIND(Q), Q.\n");
    myFixture.addFileToProject("d1/lib1/include/s.hrl", "-define(S_BIND(V), V = 1).\n");
    PsiFile c = myFixture.addFileToProject("d1/c.erl", "-module(c).\n-include_lib(\"lib1/include/s.hrl\").\n-export([f/0]).\nf() -> ?S_BIND(Y), {Y, W}.\n");
    PsiFile b = myFixture.addFileToProject("d1/b.erl", "-module(b).\n-define(B(X), X = 1).\n-export([g/0]).\ng() -> ?B(Q), Q.\n");
    highlight(a, late, c, b);
    assertEquals("ErlangQVarImpl@/src/d1/a.erl", resolved(a, "Z.\n"));
    assertEquals("null", resolved(late, "Q.\n"));
    assertEquals("null", resolved(c, "W}"));
    int b0 = builds().size();
    out("D1 initial " + reasons());

    // 1. files that no include resolves to: created, renamed, moved, deleted; also a header with another name
    VirtualFile junk = myFixture.addFileToProject("junk/unrelated.erl", "-module(unrelated).\n").getVirtualFile();
    myFixture.addFileToProject("junk/other.hrl", "-define(OTHER, 1).\n");
    myFixture.addFileToProject("d1/sub/x.erl", "-module(x).\n");
    VirtualFile gone = myFixture.addFileToProject("junk/gone/y.erl", "-module(y).\n").getVirtualFile().getParent();
    WriteAction.run(() -> {
      junk.rename(this, "unrelated2.erl");
      VirtualFile dir = junk.getParent().createChildDirectory(this, "moved");
      junk.move(this, dir); // the platform clears the caches of every PSI file: their modification stamps move
      junk.delete(this);
      gone.delete(this); // a directory with a file: the same
    });
    highlight(a, late, c, b);
    int b1 = builds().size();
    out("D1 unrelated file events: rebuilds=" + (b1 - b0) + " " + reasons());
    assertEquals("unrelated file events rebuilt copies: " + reasons(), 0, b1 - b0);

    // 2. a file with the name of an included header appears where no include looks: nothing changes
    myFixture.addFileToProject("junk/h.hrl", "-define(H_BIND(V), V = 0, Extra = 1).\n");
    myFixture.addFileToProject("apps/lib1/include/s.hrl", "-define(S_BIND(V), V = 2, W = 3).\n");
    highlight(a, late, c, b);
    int b2 = builds().size();
    out("D1 same names elsewhere: rebuilds=" + (b2 - b1) + " " + reasons());
    assertEquals(0, b2 - b1);

    // 3. the missing header of late.erl appears: only late.erl gets a (first) copy
    myFixture.addFileToProject("d1/late.hrl", "-define(L_BIND(V), V = 1).\n");
    highlight(a, late, c, b);
    int b3 = builds().size();
    out("D1 late.hrl appears: builds=" + (b3 - b2) + " " + reasons());
    assertEquals(1, b3 - b2);
    assertEquals(1, buildsOf("late.erl"));
    assertEquals("ErlangQVarImpl@/src/d1/late.erl", resolved(late, "Q.\n"));

    // 4. an application directory lib1 appears: c.erl's include_lib now resolves to apps/lib1/include/s.hrl
    myFixture.addFileToProject("apps/lib1/ebin/lib1.app", "{application, lib1, [{vsn, \"1\"}]}.\n");
    highlight(a, late, c, b);
    int b4 = builds().size();
    out("D1 lib1 application appears: builds=" + (b4 - b3) + " " + reasons());
    assertEquals(1, b4 - b3);
    assertEquals(2, buildsOf("c.erl"));
    assertEquals("ErlangMacroLayerHeaderTarget@/src/apps/lib1/include/s.hrl", resolved(c, "W}"));

    // 5. the header of a.erl is renamed away: a.erl falls back to master (no copy); then it comes back
    VirtualFile h = myFixture.findFileInTempDir("d1/h.hrl");
    WriteAction.run(() -> h.rename(this, "h_old.hrl"));
    highlight(a, late, c, b);
    int b5 = builds().size();
    out("D1 h.hrl renamed away: builds=" + (b5 - b4) + " " + reasons());
    assertEquals(0, b5 - b4);
    assertEquals("null", resolved(a, "Z.\n"));
    WriteAction.run(() -> h.rename(this, "h.hrl"));
    highlight(a, late, c, b);
    int b6 = builds().size();
    out("D1 h.hrl renamed back: builds=" + (b6 - b5) + " " + reasons());
    assertEquals(1, b6 - b5);
    assertEquals("ErlangQVarImpl@/src/d1/a.erl", resolved(a, "Z.\n"));
    assertEquals(1, buildsOf("b.erl"));
  }

  /**
   * An edit and its exact reverse (as undo does): the text is the old one, but the reparse made new elements. The copy
   * must not keep answers that point to the replaced elements, and an unrelated move must still rebuild nothing.
   */
  public void testTextBackToAnEarlierTextGivesValidAnswers() throws Exception {
    // round 5: the question comes after the user paused (right after an edit only highlighting builds the copy)
    ErlangMacroLayer.setQuietMillisForTests(0, getTestRootDisposable());
    PsiFile a = myFixture.addFileToProject("d1b/a.erl", "-module(a).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Q), Q.\n");
    highlight(a);
    assertEquals("ErlangQVarImpl@/src/d1b/a.erl", resolved(a, "Q.\n"));
    com.intellij.openapi.editor.Document d = com.intellij.psi.PsiDocumentManager.getInstance(getProject()).getDocument(a);
    String text = a.getText();
    com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      d.setText("-module(a).\n");
      com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitDocument(d);
      d.setText(text);
      com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitDocument(d);
    });
    String valid = inBackground(() -> {
      PsiElement t = refAt(a, "Q.\n").resolve();
      return t == null ? "null" : t.isValid() ? "valid" : "INVALID " + t;
    });
    out("D1b after edit and reverse: " + valid + " " + reasons());
    assertEquals("valid", valid);
  }

  // ------------------------------------------------------------------------------------------------ defect 3

  /** Runs on a pooled thread without a read action, like listeners and expiration checks. */
  private static <T> T withoutReadAction(java.util.concurrent.Callable<T> c) throws Exception {
    return com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(() -> {
      assertFalse(com.intellij.openapi.application.ApplicationManager.getApplication().isReadAccessAllowed());
      return c.call();
    }).get();
  }

  private PsiElement headerTarget(PsiFile includer, String marker) throws Exception {
    PsiElement t = inBackground(() -> refAt(includer, marker).resolve());
    assertTrue(String.valueOf(t), t instanceof ErlangMacroLayerHeaderTarget);
    return t;
  }

  private void editHeader(PsiFile h, String from, String to) {
    com.intellij.openapi.editor.Document d = com.intellij.psi.PsiDocumentManager.getInstance(getProject()).getDocument(h);
    int at = h.getText().indexOf(from);
    com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.replaceString(at, at + from.length(), to));
    com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  /** P6: isValid() of a header target answers without a read action (as PSI files do) and reads no PSI there. */
  public void testHeaderTargetIsValidWithoutReadAction() throws Exception {
    PsiFile h = myFixture.addFileToProject("d3/h.hrl", "-define(H_X, Xh = 1).\n");
    PsiFile a = myFixture.addFileToProject("d3/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([g/0]).\ng() -> ?H_X, Xh.\n");
    highlight(a);
    PsiElement t = headerTarget(a, "Xh.\n");
    assertTrue(withoutReadAction(t::isValid));
    assertTrue(withoutReadAction(((ErlangMacroLayerHeaderTarget)t)::canNavigate));
    // the name leaves the header's directive: invalid, under a read action and then without one
    editHeader(h, "Xh = 1", "Yh = 1");
    assertFalse(inBackground(t::isValid));
    assertFalse(withoutReadAction(t::isValid));
    editHeader(h, "Yh = 1", "Xh = 1");
    assertTrue(inBackground(t::isValid));
    assertTrue(withoutReadAction(t::isValid));
    // the header is deleted
    WriteAction.run(() -> h.getVirtualFile().delete(this));
    assertFalse(withoutReadAction(t::isValid));
    assertFalse(inBackground(t::isValid));
  }

  // ------------------------------------------------------------------------------------------------ defect 4

  /** P6: the rename dialog, conflict texts and usage views name a header target by its kind, not by a class name. */
  public void testHeaderTargetHasAHumanName() throws Exception {
    myFixture.addFileToProject("d4/h.hrl", "-define(H_X, Xh = 1).\n");
    PsiFile a = myFixture.addFileToProject("d4/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([g/0]).\ng() -> ?H_X, Xh.\n");
    highlight(a);
    PsiElement t = headerTarget(a, "Xh.\n");
    String texts = inBackground(() -> String.join(" | ",
      // the label of the rename dialog: RenameDialog.getFullName()
      com.intellij.usageView.UsageViewUtil.getType(t) + " " + com.intellij.lang.findUsages.DescriptiveNameUtil.getDescriptiveName(t),
      com.intellij.refactoring.util.RefactoringUIUtil.getDescription(t, true),
      com.intellij.refactoring.util.RefactoringUIUtil.getDescription(t, false),
      com.intellij.usageView.UsageViewUtil.getLongName(t),
      com.intellij.usageView.UsageViewUtil.getShortName(t),
      com.intellij.psi.ElementDescriptionUtil.getElementDescription(t, com.intellij.usageView.UsageViewNodeTextLocation.INSTANCE),
      com.intellij.psi.ElementDescriptionUtil.getElementDescription(t, com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation.INSTANCE)));
    out("D4 " + texts);
    assertFalse(texts, texts.contains("Macro Layer") || texts.contains("Header Target") || texts.contains("HeaderTarget"));
    assertEquals("variable", inBackground(() -> com.intellij.usageView.UsageViewUtil.getType(t)));
    assertTrue(texts, texts.startsWith("variable Xh | "));
  }

  // ------------------------------------------------------------------------------------------------ defect 2

  private String usagesText(java.util.Collection<? extends com.intellij.usageView.UsageInfo> usages) throws Exception {
    return inBackground(() -> {
      List<String> r = new java.util.ArrayList<>();
      for (com.intellij.usageView.UsageInfo u : usages) {
        PsiElement e = u.getElement();
        PsiFile f = e == null ? null : e.getContainingFile();
        int line = f == null ? -1 : com.intellij.openapi.util.text.StringUtil.offsetToLineNumber(f.getText(), e.getTextRange().getStartOffset()) + 1;
        r.add((f == null ? "?" : f.getName()) + ":" + line + ":" + (e == null ? null : e.getText()));
      }
      java.util.Collections.sort(r);
      return r.toString();
    });
  }

  /**
   * The Find Usages action as the IDE runs it: the platform picks the handler, then the search runs on a pooled thread.
   * (The fixture's findUsages searches on the UI thread, where the layer answers only from up-to-date caches.)
   */
  private String findUsagesLikeTheIde(PsiElement t) throws Exception {
    com.intellij.find.findUsages.FindUsagesManager manager =
      ((com.intellij.find.impl.FindManagerImpl)com.intellij.find.FindManager.getInstance(getProject())).getFindUsagesManager();
    com.intellij.find.findUsages.FindUsagesHandler handler =
      manager.getFindUsagesHandler(t, com.intellij.find.findUsages.FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    assertNotNull("Cannot find handler for: " + t, handler);
    com.intellij.find.findUsages.FindUsagesOptions options = handler.getFindUsagesOptions(null);
    return usagesText(com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(() -> {
      com.intellij.util.CommonProcessors.CollectProcessor<com.intellij.usageView.UsageInfo> p =
        new com.intellij.util.CommonProcessors.CollectProcessor<>(java.util.Collections.synchronizedList(new java.util.ArrayList<>()));
      for (PsiElement e : handler.getPrimaryElements()) handler.processElementUsages(e, p, options);
      return p.getResults();
    }).get());
  }

  /** P6: find usages on a name whose target is in the body of a header macro finds the uses in every includer. */
  public void testFindUsagesOfAHeaderTarget() throws Exception {
    myFixture.addFileToProject("d2/h.hrl", "-define(H_X, Xh = 1).\n");
    PsiFile a = myFixture.addFileToProject("d2/a.erl",
      "-module(a).\n-include(\"h.hrl\").\n-export([g/0, k/0]).\ng() -> ?H_X, Xh.\nk() -> ?H_X,\n  {Xh, Xh}.\n");
    PsiFile a2 = myFixture.addFileToProject("d2/a2.erl", "-module(a2).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?H_X, Xh + 1.\n");
    PsiFile b = myFixture.addFileToProject("d2/b.erl", "-module(b).\n-export([z/0]).\nz() -> Xh = 2, Xh.\n");
    highlight(a, a2, b);
    PsiElement t = headerTarget(a, "Xh.\n");
    // in the IDE: the search runs in the background
    String background = usagesText(inBackground(() -> {
      List<com.intellij.usageView.UsageInfo> r = new java.util.ArrayList<>();
      for (PsiReference ref : com.intellij.psi.search.searches.ReferencesSearch.search(t).findAll()) r.add(new com.intellij.usageView.UsageInfo(ref));
      return r;
    }));
    out("D2 references search: " + background);
    String action = findUsagesLikeTheIde(t);
    out("D2 find usages: " + action);
    assertEquals("[a.erl:4:Xh, a.erl:6:Xh, a.erl:6:Xh, a2.erl:4:Xh]", action);
    assertEquals(action, background);
  }

  /** P6: find usages on a function made by a header macro ({@code ?GEN(foo).}) finds its local calls. */
  public void testFindUsagesOfAFunctionMadeByAHeaderMacro() throws Exception {
    myFixture.addFileToProject("d2f/h.hrl", "-define(GEN(N), N() -> ok).\n");
    PsiFile a = myFixture.addFileToProject("d2f/a.erl",
      "-module(a).\n-include(\"h.hrl\").\n-export([g/0, foo/0]).\n?GEN(foo).\ng() -> foo().\nk() ->\n  {foo(), foo}.\n");
    PsiFile b = myFixture.addFileToProject("d2f/b.erl", "-module(b).\n-export([foo/0]).\nfoo() -> foo.\nz() -> foo().\n");
    highlight(a, b);
    PsiElement t = inBackground(() -> refAt(a, "foo().\nk").resolve());
    assertNotNull(t);
    assertEquals(a.getText().indexOf("?GEN(foo)") + 5, (int)inBackground(() -> t.getTextRange().getStartOffset()));
    String action = findUsagesLikeTheIde(t);
    out("D2 find usages of foo: " + action);
    // the find usages view: "Usages of function foo", not of a "Leaf Psi Element"
    String names = inBackground(() -> com.intellij.usageView.UsageViewUtil.getType(t) + " " + com.intellij.lang.findUsages.DescriptiveNameUtil.getDescriptiveName(t) +
                                      " | " + com.intellij.refactoring.util.RefactoringUIUtil.getDescription(t, false));
    out("D2 names of foo: " + names);
    assertEquals("function foo | function <b><code>foo</code></b>", names);
    assertEquals("[a.erl:3:foo/0, a.erl:5:foo(), a.erl:7:foo()]", action);
  }

  // ------------------------------------------------------------------------------------------------ defect 6

  private static <T> java.util.concurrent.Future<T> pooled(java.util.concurrent.Callable<T> c) {
    return com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(c);
  }

  /** A pooled thread asks for the copy in a read action under its own progress indicator: "copy", "none" or "cancelled". */
  private static java.util.concurrent.Future<String> askUnder(com.intellij.openapi.progress.ProgressIndicator indicator, PsiFile f) {
    return pooled(() -> {
      try {
        ErlangMacroLayer.Copy[] c = new ErlangMacroLayer.Copy[1];
        com.intellij.openapi.progress.ProgressManager.getInstance().runProcess(
          () -> c[0] = com.intellij.openapi.application.ReadAction.compute(() -> ErlangMacroLayer.getCopy(f)), indicator);
        return c[0] != null ? "copy" : "none";
      }
      catch (com.intellij.openapi.progress.ProcessCanceledException e) {
        return "cancelled";
      }
    });
  }

  /**
   * C5/P3: a thread that waits for another thread's build of the same copy stays cancellable, and the copy is built once.
   * A waiter holds a read action; a write action must be able to cancel it at once, not after the other thread's build.
   */
  public void testWaitingForAnotherThreadsBuildIsCancellable() throws Exception {
    PsiFile f = myFixture.addFileToProject("d6/a.erl", "-module(a).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Q), Q.\n");
    java.util.concurrent.CountDownLatch entered = new java.util.concurrent.CountDownLatch(1);
    java.util.concurrent.CountDownLatch release = new java.util.concurrent.CountDownLatch(1);
    java.util.concurrent.atomic.AtomicInteger hookRuns = new java.util.concurrent.atomic.AtomicInteger();
    ErlangMacroLayer.setBuildHookForTests(() -> {
      if (hookRuns.incrementAndGet() > 1) return;
      entered.countDown();
      try {
        // the first build hangs until released (a slow build), but reacts to cancellation like a real one
        while (!release.await(5, java.util.concurrent.TimeUnit.MILLISECONDS)) com.intellij.openapi.progress.ProgressManager.checkCanceled();
      }
      catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    }, getTestRootDisposable());
    com.intellij.openapi.progress.util.ProgressIndicatorBase builderIndicator = new com.intellij.openapi.progress.util.ProgressIndicatorBase();
    com.intellij.openapi.progress.util.ProgressIndicatorBase waiterIndicator = new com.intellij.openapi.progress.util.ProgressIndicatorBase();
    java.util.concurrent.Future<String> builder = askUnder(builderIndicator, f);
    java.util.concurrent.Future<String> waiter = null;
    java.util.concurrent.Future<String> second = null;
    try {
      assertTrue("the build did not start", entered.await(30, java.util.concurrent.TimeUnit.SECONDS));
      waiter = askUnder(waiterIndicator, f);
      Thread.sleep(300); // the waiter reaches the wait
      assertFalse("the waiter did not wait for the build", waiter.isDone());
      long t0 = System.nanoTime();
      waiterIndicator.cancel();
      String w;
      try {
        w = waiter.get(5, java.util.concurrent.TimeUnit.SECONDS);
      }
      catch (java.util.concurrent.TimeoutException e) {
        w = "still waiting after 5 s";
      }
      double ms = (System.nanoTime() - t0) / 1e6;
      out("D6 cancelled waiter: " + w + " after " + String.format(java.util.Locale.ROOT, "%.1f", ms) + " ms; builder done=" + builder.isDone());
      assertEquals("cancelled", w);
      assertFalse("the build ended before the waiter", builder.isDone());
      // a second waiter, not cancelled, gets the copy of the first build when it ends
      second = askUnder(new com.intellij.openapi.progress.util.ProgressIndicatorBase(), f);
      Thread.sleep(100);
      release.countDown();
      assertEquals("copy", builder.get(30, java.util.concurrent.TimeUnit.SECONDS));
      assertEquals("copy", second.get(30, java.util.concurrent.TimeUnit.SECONDS));
      out("D6 builds=" + buildsOf("a.erl") + " " + reasons());
      assertEquals(1, buildsOf("a.erl"));
    }
    finally {
      release.countDown();
      for (java.util.concurrent.Future<String> x : java.util.Arrays.asList(builder, waiter, second)) {
        if (x != null) {
          try {
            x.get(30, java.util.concurrent.TimeUnit.SECONDS);
          }
          catch (Exception ignored) {
          }
        }
      }
    }
  }

  /** When the building thread is cancelled, a waiting thread builds the copy itself; 8 threads at once build once. */
  public void testCancelledBuildIsTakenOverAndConcurrentAsksBuildOnce() throws Exception {
    PsiFile f = myFixture.addFileToProject("d6b/a.erl", "-module(a).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Q), Q.\n");
    java.util.concurrent.CountDownLatch entered = new java.util.concurrent.CountDownLatch(1);
    java.util.concurrent.atomic.AtomicInteger hookRuns = new java.util.concurrent.atomic.AtomicInteger();
    ErlangMacroLayer.setBuildHookForTests(() -> {
      if (hookRuns.incrementAndGet() > 1) return;
      entered.countDown();
      long until = System.nanoTime() + 30_000_000_000L;
      while (System.nanoTime() < until) {
        com.intellij.openapi.progress.ProgressManager.checkCanceled();
        Thread.onSpinWait();
      }
    }, getTestRootDisposable());
    com.intellij.openapi.progress.util.ProgressIndicatorBase builderIndicator = new com.intellij.openapi.progress.util.ProgressIndicatorBase();
    java.util.concurrent.Future<String> builder = askUnder(builderIndicator, f);
    assertTrue(entered.await(30, java.util.concurrent.TimeUnit.SECONDS));
    java.util.concurrent.Future<String> waiter = askUnder(new com.intellij.openapi.progress.util.ProgressIndicatorBase(), f);
    Thread.sleep(200);
    builderIndicator.cancel();
    assertEquals("cancelled", builder.get(30, java.util.concurrent.TimeUnit.SECONDS));
    assertEquals("copy", waiter.get(30, java.util.concurrent.TimeUnit.SECONDS));
    assertEquals(1, buildsOf("a.erl"));
    // 8 threads ask a fresh file at once
    PsiFile g = myFixture.addFileToProject("d6b/b.erl", "-module(b).\n-define(B(X), X = 1).\n-export([f/0]).\nf() -> ?B(Q), Q.\n");
    java.util.concurrent.CyclicBarrier start = new java.util.concurrent.CyclicBarrier(8);
    List<java.util.concurrent.Future<String>> all = new java.util.ArrayList<>();
    for (int i = 0; i < 8; i++) {
      all.add(pooled(() -> {
        start.await(30, java.util.concurrent.TimeUnit.SECONDS);
        return askUnder(new com.intellij.openapi.progress.util.ProgressIndicatorBase(), g).get(30, java.util.concurrent.TimeUnit.SECONDS);
      }));
    }
    for (java.util.concurrent.Future<String> x : all) assertEquals("copy", x.get(60, java.util.concurrent.TimeUnit.SECONDS));
    out("D6 eight threads: builds=" + buildsOf("b.erl") + " " + reasons());
    assertEquals(1, buildsOf("b.erl"));
  }
}
