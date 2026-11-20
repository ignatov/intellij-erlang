package org.intellij.erlang.macrolayer;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/** Spike 4: the count rules C1 to C5, read from the build log. */
public class ErlangMacroLayerCountTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private int myN;

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
      List<String> ui = uiThreadBuilds();
      out(getName() + " C5 ui-thread builds=" + ui.size() + " " + ui);
      out(getName() + " queries " + ErlangMacroLayerLog.queries());
      assertEquals("C5: a copy was built on the UI thread", List.of(), ui);
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
    System.out.println("LAYER-COUNT " + s);
  }

  private void highlight(PsiFile f) {
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
  }

  private void typeAndCommit(String s) {
    for (char c : s.toCharArray()) {
      myFixture.type(c);
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
  }

  private void edit(PsiFile f, int offset, String text) {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(offset, text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  private static String reasons(List<ErlangMacroLayerLog.Event> b) {
    return b.stream().map(e -> e.file() + ":" + e.reason() + ":" + e.trigger()).toList().toString();
  }

  // ------------------------------------------------------------------------------------------------ C1

  public void testC1NothingToSubstitute() throws Exception {
    myFixture.addFileToProject("c1/h1.hrl", "-define(H1(X), {X}).\n-define(H1B, ok).\n");
    List<PsiFile> files = new ArrayList<>();
    files.add(myFixture.addFileToProject("c1/plain.erl", "-module(plain).\n-export([f/1]).\nf(X) -> Y = X + 1, g(Y).\ng(A) -> A.\nh() -> undefined_call(Z).\n"));
    files.add(myFixture.addFileToProject("c1/predef.erl", "-module(predef).\n-export([f/0]).\nf() -> {?MODULE, ?LINE, ?FUNCTION_NAME, ?NOT_DEFINED_ANYWHERE}.\n"));
    files.add(myFixture.addFileToProject("c1/incnouse.erl", "-module(incnouse).\n-include(\"h1.hrl\").\n-export([f/0]).\nf() -> X = 1, X + Y.\n"));
    files.add(myFixture.addFileToProject("c1/defnouse.erl", "-module(defnouse).\n-define(D(X), X).\n-export([f/0]).\nf() -> Q + 1.\n"));
    String list = env("C1_FILES");
    if (list != null) {
      for (String l : Files.readAllLines(Paths.get(list))) {
        if (l.isBlank()) continue;
        Path p = Paths.get(l.trim());
        files.add(myFixture.addFileToProject("c1/corpus" + (myN++) + "/" + p.getFileName(), read(p)));
      }
    }
    for (PsiFile f : files) {
      int before = builds().size();
      highlight(f);
      myFixture.getEditor().getCaretModel().moveToOffset(f.getTextLength());
      typeAndCommit("\n%% t\nzz() -> ok.\n");
      myFixture.doHighlighting();
      int resolved = resolveAll(myFixture.getFile());
      // navigation on the UI thread, at every variable
      for (ErlangQVar v : com.intellij.psi.util.PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class)) {
        if (v.getReference() != null) v.getReference().resolve();
      }
      int after = builds().size();
      out("C1 " + f.getName() + " builds=" + (after - before) + " resolvedRefs=" + resolved);
      assertEquals("C1 " + f.getName() + " " + reasons(builds().subList(before, after)), 0, after - before);
    }
  }

  // ------------------------------------------------------------------------------------------------ C2

  public void testC2OneBuildPerModificationAndOnlyWhenAsked() throws Exception {
    PsiFile f = myFixture.addFileToProject("c2/c2.erl",
      "-module(c2).\n-export([f/0, g/0]).\n-define(BIND(V), V = 1).\n-define(PAIR(A, B), {A, B}).\n" +
      "f() -> ?BIND(Xa), Xa + 1.\n" +
      "g() -> ?PAIR(1, 2).\n");
    highlight(f);
    int b0 = builds().size();
    out("C2 first highlighting builds=" + b0);
    assertTrue(b0 <= 1);
    myFixture.getEditor().getCaretModel().moveToOffset(f.getText().indexOf("?PAIR(1, 2)") + "?PAIR(1".length());
    typeAndCommit("0000");
    int b1 = builds().size();
    out("C2 4 keystrokes with commit, no question: builds=" + (b1 - b0));
    assertEquals(0, b1 - b0);
    myFixture.doHighlighting();
    int b2 = builds().size();
    out("C2 highlighting after 4 modifications: builds=" + (b2 - b1));
    assertTrue(b2 - b1 <= 1);
    for (int i = 0; i < 5; i++) {
      typeAndCommit("1");
      myFixture.doHighlighting();
      resolveAll(myFixture.getFile());
      myFixture.doHighlighting();
    }
    int b3 = builds().size();
    out("C2 5 x (keystroke, highlighting, resolve all, highlighting): builds=" + (b3 - b2) + " " + reasons(builds().subList(b2, b3)));
    assertTrue(b3 - b2 <= 5);
  }

  // ------------------------------------------------------------------------------------------------ C3

  public void testC3EditsElsewhere() throws Exception {
    PsiFile h = myFixture.addFileToProject("c3/h.hrl",
      "%% header\n-define(H_BIND(V), V = 42).\n-define(H_X, Xh = 1).\nh_helper() -> ok.\n");
    PsiFile a = myFixture.addFileToProject("c3/a.erl",
      "-module(a).\n-include(\"h.hrl\").\n-export([f/0, g/0]).\nf() -> ?H_BIND(Z), Z.\ng() -> ?H_X, Xh.\n");
    PsiFile a2 = myFixture.addFileToProject("c3/a2.erl",
      "-module(a2).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?H_BIND(Z), Z.\n");
    PsiFile b = myFixture.addFileToProject("c3/b.erl",
      "-module(b).\n-define(B(X), X = 1).\n-export([g/0]).\ng() -> ?B(Q), Q.\n");
    highlight(a);
    highlight(a2);
    highlight(b);
    int b0 = builds().size();
    out("C3 initial builds=" + b0 + " " + reasons(builds()));
    // go-to of Xh lands in the header body (lazy target)
    PsiElement target = inBackground(() -> varRef(a, "Xh.\n").resolve());
    out("C3 Xh target=" + target);
    assertTrue(String.valueOf(target), target instanceof ErlangMacroLayerHeaderTarget);
    // 1. an unrelated file
    edit(b, b.getTextLength(), "\nh() -> ok.\n");
    highlight(a);
    highlight(a2);
    int b1 = builds().size();
    out("C3 edit in an unrelated file: rebuilds of includers=" + (b1 - b0));
    assertEquals(0, b1 - b0);
    // 2. the header, outside its directive lines (above them: the offsets of the directives move)
    edit(h, 0, "%% a new comment line above the directives\n");
    edit(h, h.getText().indexOf("ok.\n"), "ok, ");
    highlight(a);
    highlight(a2);
    int b2 = builds().size();
    out("C3 header edits outside the directive lines: rebuilds=" + (b2 - b1) + " " + reasons(builds().subList(b1, b2)));
    assertEquals(0, b2 - b1);
    PsiElement t2 = inBackground(() -> varRef(a, "Xh.\n").resolve());
    com.intellij.openapi.util.TextRange r = inBackground(t2::getTextRange);
    String atRange = r == null ? null : r.subSequence(h.getText()).toString();
    out("C3 header target after the shift: " + t2 + " range=" + r + " text=" + atRange);
    assertEquals("Xh", atRange);
    PsiElement nav = inBackground(t2::getNavigationElement);
    out("C3 navigation element=" + nav + " in " + nav.getContainingFile().getName() + " at " + nav.getTextRange());
    assertEquals("Xh", nav.getText());
    // 3. a directive line of the header
    edit(h, h.getText().indexOf("42"), "1");
    highlight(a);
    int b3 = builds().size();
    out("C3 directive edit, includer a asked: builds=" + (b3 - b2) + " " + reasons(builds().subList(b2, b3)));
    assertEquals(1, b3 - b2);
    assertEquals("header-directives", builds().get(b3 - 1).reason());
    int a2before = (int)builds().stream().filter(e -> e.file().equals("a2.erl")).count();
    highlight(b);
    int a2mid = (int)builds().stream().filter(e -> e.file().equals("a2.erl")).count();
    out("C3 a2 not asked yet: builds=" + (a2mid - a2before));
    assertEquals(0, a2mid - a2before);
    highlight(a2);
    int a2after = (int)builds().stream().filter(e -> e.file().equals("a2.erl")).count();
    out("C3 a2 asked: builds=" + (a2after - a2mid));
    assertEquals(1, a2after - a2mid);
    out("C3 summaries built=" + count("SUMMARY") + " (one per header text version)");
  }

  private static PsiReference varRef(PsiFile f, String marker) {
    int at = f.getText().indexOf(marker);
    return f.findReferenceAt(at);
  }

  // ------------------------------------------------------------------------------------------------ C4

  public void testC4SwitchBetween20OpenFiles() throws Exception {
    String list = env("OPEN20_FILES");
    if (list == null) return;
    List<VirtualFile> vfs = new ArrayList<>();
    for (String l : Files.readAllLines(Paths.get(list))) {
      if (l.isBlank()) continue;
      Path p = Paths.get(l.trim());
      vfs.add(myFixture.addFileToProject("open" + (myN++) + "/" + p.getFileName(), read(p)).getVirtualFile());
    }
    for (VirtualFile v : vfs) {
      myFixture.configureFromExistingVirtualFile(v);
      myFixture.doHighlighting();
    }
    int b0 = builds().size();
    for (int round = 0; round < 3; round++) {
      for (VirtualFile v : vfs) {
        myFixture.configureFromExistingVirtualFile(v);
        myFixture.doHighlighting();
      }
    }
    int b1 = builds().size();
    out("C4 files=" + vfs.size() + " first pass builds=" + b0 + " then 3 switching rounds without edit: builds=" + (b1 - b0) +
        " strongly held copies=" + ErlangMacroLayerRetention.getInstance(getProject()).strongCount());
    assertEquals(reasons(builds().subList(b0, b1)), 0, b1 - b0);
  }

  // ------------------------------------------------------------------------------------------------ C5

  public void testC5UiThreadNeverBuilds() throws Exception {
    PsiFile f = myFixture.addFileToProject("c5/c5.erl",
      "-module(c5).\n-export([f/0]).\n-define(BIND(V), V = 1).\nf() -> ?BIND(Xa), Xa + 1.\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    assertTrue(ApplicationManager.getApplication().isDispatchThread());
    PsiReference ref = varRef(f, "Xa + 1");
    PsiElement onUi = ref.resolve();
    Object copyOnUi = ErlangMacroLayer.getCopy(f);
    out("C5 UI thread before any build: resolve=" + onUi + " copy=" + copyOnUi + " builds=" + builds().size() +
        " EDT_MISS=" + count("EDT_MISS"));
    assertNull(copyOnUi);
    assertEquals(0, builds().size());
    myFixture.doHighlighting();
    PsiElement afterHl = varRef(f, "Xa + 1").resolve();
    out("C5 explain " + ErlangMacroLayer.explainForTests(varRef(f, "Xa + 1")) + " bg=" + inBackground(() -> varRef(f, "Xa + 1").resolve()));
    out("C5 UI thread after highlighting: resolve=" + afterHl + " builds=" + builds().size() + " thread of the build=" +
        (builds().isEmpty() ? "-" : builds().getFirst().thread()));
    assertNotNull("the cached copy answers on the UI thread", afterHl);
  }

  public void testC5CancelledBuildStops() throws Exception {
    String path = env("C5_FILE");
    if (path == null) return;
    Path p = Paths.get(path);
    PsiFile f = myFixture.addFileToProject("c5big/" + p.getFileName(), read(p));
    List<Double> full = new ArrayList<>(), stop = new ArrayList<>();
    for (int i = 0; i < 8; i++) {
      long t0 = System.nanoTime();
      inBackground(() -> ErlangMacroLayer.build(f));
      if (i > 0) full.add((System.nanoTime() - t0) / 1e6);
    }
    for (int i = 0; i < 9; i++) {
      ProgressIndicatorBase indicator = new ProgressIndicatorBase();
      long[] end = new long[1];
      boolean[] built = new boolean[1];
      Future<?> fut = ApplicationManager.getApplication().executeOnPooledThread(() -> {
        try {
          ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
            ErlangMacroLayer.build(f);
            built[0] = true;
          }), indicator);
        }
        catch (ProcessCanceledException ignored) {
        }
        finally {
          end[0] = System.nanoTime();
        }
      });
      Thread.sleep(20 + 15 * i);
      long cancelAt = System.nanoTime();
      indicator.cancel();
      boolean cancelled = false;
      try {
        fut.get();
      }
      catch (ExecutionException e) {
        cancelled = e.getCause() instanceof ProcessCanceledException;
      }
      cancelled |= !built[0];
      if (cancelled) stop.add((end[0] - cancelAt) / 1e6);
      out("C5 cancel after " + (20 + 15 * i) + " ms: cancelled=" + cancelled + " stopped after " + String.format("%.2f", (end[0] - cancelAt) / 1e6) + " ms");
    }
    out("C5 " + p.getFileName() + " size=" + Files.size(p) + " full build ms " + stats(full) + "; time from cancel to stop ms " +
        stats(stop) + " load=" + load());
    assertFalse(stop.isEmpty());
  }

  @SuppressWarnings("unused")
  private static void unused(Disposable d) {
    Disposer.dispose(d);
  }
}
