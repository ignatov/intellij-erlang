package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.concurrency.JobLauncher;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesManager;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.refactoring.rename.RenameProcessor;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageInfo;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.Callable;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: while a file is typed in, only passive queries at the caret are refused a copy build; every other caller
 * builds the copy and waits for it. A search started right after a key press gets the layer's answers also when no
 * action class is on its stack: a reference search from a pooled thread, a search over worker threads, and a rename
 * whose target is found after the key press. A search that is wrongly taken for a passive query still gets them,
 * because the layer's reference searcher builds the copies of the files just typed in before the answers are read.
 * The real identifier highlighting of the platform stays refused. The quiet time is left at its default (1 s).
 */
public class ErlangMacroLayerActiveCallersTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final Map<String, PsiFile> myFiles = new LinkedHashMap<>();

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
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, read(Paths.get("testData/macroLayer/nav", n))));
    }
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
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

  private PsiElement target(String file, String marker) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      return ref == null ? null : ref.resolve();
    });
  }

  /** A key press: one character in a comment at the end of each file, committed. Returns System.nanoTime() of it. */
  private long typeInto(String... files) {
    for (String file : files) {
      Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFiles.get(file));
      WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    }
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    return System.nanoTime();
  }

  private static String where(Collection<? extends PsiElement> elements) {
    Set<String> r = new TreeSet<>();
    for (PsiElement e : elements) {
      PsiFile f = e.getContainingFile();
      r.add(f.getName() + ":" + (StringUtil.offsetToLineNumber(f.getText(), e.getTextRange().getStartOffset()) + 1));
    }
    return String.join(" ", r);
  }

  private static String refs(Collection<PsiReference> refs) {
    List<PsiElement> e = new ArrayList<>();
    for (PsiReference r : refs) e.add(r.getElement());
    return where(e);
  }

  private static String usages(UsageInfo[] usages) {
    List<PsiElement> e = new ArrayList<>();
    for (UsageInfo u : usages) if (u.getElement() != null) e.add(u.getElement());
    return where(e);
  }

  /** A reference search started on a pooled thread under a progress indicator: no action class is on its stack. */
  private static String searchFromAPooledThread(PsiElement target) throws Exception {
    return inBackgroundWithIndicator(() -> refs(ReferencesSearch.search(target).findAll()));
  }

  /**
   * One search per file, each on its own worker thread of the job launcher, under the indicator of a pooled thread
   * that started them: the workers' stacks show neither the action nor the starting thread.
   */
  private String searchOnWorkerThreads(PsiElement target, String... files) throws Exception {
    List<PsiReference> found = Collections.synchronizedList(new ArrayList<>());
    Set<String> threads = Collections.synchronizedSet(new TreeSet<>());
    List<PsiFile> list = new ArrayList<>();
    for (String f : files) list.add(myFiles.get(f));
    ProgressIndicator indicator = new EmptyProgressIndicator();
    ApplicationManager.getApplication().executeOnPooledThread(() -> ProgressManager.getInstance().runProcess(() -> {
      Thread caller = Thread.currentThread();
      JobLauncher.getInstance().invokeConcurrentlyUnderProgress(list, indicator, f -> ReadAction.compute(() -> {
        threads.add(Thread.currentThread() == caller ? "caller" : "worker");
        found.addAll(ReferencesSearch.search(target, new LocalSearchScope(f)).findAll());
        return true;
      }));
    }, indicator)).get();
    System.out.println("LAYER-AC search threads: " + threads);
    return inBackgroundWithIndicator(() -> refs(found));
  }

  private static void assertInsideTheQuietTime(long typedAt) {
    long ms = (System.nanoTime() - typedAt) / 1_000_000;
    System.out.println("LAYER-AC search ended " + ms + " ms after the key press");
    assertTrue("the search must end inside the quiet time: " + ms + " ms", ms < ErlangMacroLayer.quietMillis());
  }

  /** The usage search of rename, as BaseRefactoringProcessor runs it: findUsages() in the background. */
  private static String renameUsages(PsiElement target, String newName, boolean wronglyPassive) throws Exception {
    RenameProcessor p = new RenameProcessor(target.getProject(), target, newName, false, false);
    UsageInfo[] found = inBackgroundWithIndicator(maybePassive(p::findUsages, wronglyPassive));
    return inBackgroundWithIndicator(() -> usages(found));
  }

  /** Find usages as the IDE runs it: the platform's handler and usage searcher, the search in the background. */
  private String findUsages(PsiElement target, boolean wronglyPassive) throws Exception {
    FindUsagesManager manager = ((FindManagerImpl)com.intellij.find.FindManager.getInstance(getProject())).getFindUsagesManager();
    FindUsagesHandler handler = manager.getFindUsagesHandler(target, FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    assertNotNull(handler);
    FindUsagesOptions options = handler.getFindUsagesOptions(null);
    com.intellij.usages.UsageSearcher searcher =
      FindUsagesManager.createUsageSearcher(handler, handler.getPrimaryElements(), handler.getSecondaryElements(), options);
    UsageInfo[] found = inBackgroundWithIndicator(maybePassive(() -> {
      List<UsageInfo> r = Collections.synchronizedList(new ArrayList<>());
      searcher.generate(u -> {
        if (u instanceof com.intellij.usages.UsageInfo2UsageAdapter a) r.add(a.getUsageInfo());
        return true;
      });
      return r.toArray(UsageInfo.EMPTY_ARRAY);
    }, wronglyPassive));
    return inBackgroundWithIndicator(() -> usages(found));
  }

  /**
   * {@code wronglyPassive}: every question of the computation is taken for a passive query (as a passive mark that
   * stayed on the indicator would make it), though no passive frame is on the stack.
   */
  private static <T> Callable<T> maybePassive(Callable<T> c, boolean wronglyPassive) {
    if (!wronglyPassive) return c;
    return () -> ErlangMacroLayer.computeAsPassiveQueryForTests(() -> {
      try {
        return c.call();
      }
      catch (RuntimeException e) {
        throw e;
      }
      catch (Exception e) {
        throw new RuntimeException(e);
      }
    });
  }

  public void testReferenceSearchFromAPooledThreadRightAfterTyping() throws Exception {
    // Z of b/0 is bound by the argument of ?BIND(Z): only the layer resolves the use after it
    PsiElement t = target("s.erl", "Z + ?CONST");
    assertNotNull(t);
    String expected = searchFromAPooledThread(t);
    assertEquals("s.erl:22", expected);
    long typed = typeInto("s.erl");
    String after = searchFromAPooledThread(t);
    assertInsideTheQuietTime(typed);
    assertEquals(expected, after);
  }

  public void testSearchOverWorkerThreadsRightAfterTyping() throws Exception {
    PsiElement t = target("i.erl", "Xh.\n");
    String expected = searchOnWorkerThreads(t, "i.erl", "i2.erl");
    assertEquals("i.erl:16 i2.erl:16", expected);
    long typed = typeInto("i.erl", "i2.erl");
    String after = searchOnWorkerThreads(t, "i.erl", "i2.erl");
    assertInsideTheQuietTime(typed);
    assertEquals(expected, after);
  }

  /** The rename starts after the key press: its target is found at the caret in the background, as the data rules of the IDE do. */
  public void testRenameWhoseTargetIsFoundAfterTheKeyPress() throws Exception {
    PsiFile s = myFiles.get("s.erl");
    myFixture.configureFromExistingVirtualFile(s.getVirtualFile());
    Editor editor = myFixture.getEditor();
    int caret = s.getText().indexOf("made(), fixed()") + 1;
    PsiElement before = inBackgroundWithIndicator(
      () -> TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret));
    assertNotNull(before);
    String expected = renameUsages(before, "made2", false);
    assertEquals("s.erl:2 s.erl:24", expected);
    long typed = typeInto("s.erl");
    PsiElement after = inBackgroundWithIndicator(
      () -> TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret));
    assertNotNull("the target at the caret right after the key press", after);
    String usages = renameUsages(after, "made2", false);
    assertInsideTheQuietTime(typed);
    assertEquals(expected, usages);
  }

  /**
   * A rename and a find usages whose every question is taken for a passive query: the layer's searcher builds the
   * copies of the files just typed in first, so the searches still find every usage. Without it the rename of Xh
   * would edit only i2.erl, and the function made by a macro would have no usages.
   */
  public void testSearchesWronglyTakenForPassiveStillGetTheLayersAnswers() throws Exception {
    PsiElement xh = target("i.erl", "Xh.\n");
    PsiElement made = target("s.erl", "made(), fixed()");
    String xhExpected = renameUsages(xh, "Xh2", false);
    String madeExpected = renameUsages(made, "made2", false);
    String findExpected = findUsages(made, false);
    assertEquals("i.erl:16 i2.erl:16", xhExpected);
    assertEquals("s.erl:2 s.erl:24", madeExpected);
    assertEquals("s.erl:2 s.erl:24", findExpected);

    long typed = typeInto("i.erl", "i2.erl", "s.erl");
    String xhAfter = renameUsages(xh, "Xh2", true);
    String madeAfter = renameUsages(made, "made2", true);
    String findAfter = findUsages(made, true);
    assertInsideTheQuietTime(typed);
    assertEquals(xhExpected, xhAfter);
    assertEquals(madeExpected, madeAfter);
    assertEquals(findExpected, findAfter);
  }

  /**
   * The identifier highlighting of the platform ({@code IdentifierHighlightingComputer}, the caller of every refused
   * question in the IDE measurement) is a passive query: right after a key press it starts no copy build and no table
   * build. It is recognised by the real stack, read once for its questions on its thread.
   */
  public void testTheIdentifierHighlightingIsRefusedByItsStack() throws Exception {
    PsiFile s = myFiles.get("s.erl");
    myFixture.configureFromExistingVirtualFile(s.getVirtualFile());
    Editor editor = myFixture.getEditor();
    int caret = s.getText().indexOf("Z + ?CONST");
    typeInto("s.erl");
    int walks0 = ErlangMacroLayer.STACK_WALKS.get();
    int searchWalks0 = ErlangMacroLayer.SEARCH_STACK_WALKS.get();
    int tables0 = ErlangMacroLayer.TABLES_BUILT.get();
    ProgressIndicator indicator = new EmptyProgressIndicator();
    ApplicationManager.getApplication().executeOnPooledThread(() -> ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
      new IdentifierHighlightingComputer(s, editor, ProperTextRange.create(0, s.getTextLength()), caret).computeRanges();
      new IdentifierHighlightingComputer(s, editor, ProperTextRange.create(0, s.getTextLength()), caret).computeRanges();
    }), indicator)).get();
    long skipped = ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals("SKIPPED") && e.line().contains("typing")).count();
    int walks = ErlangMacroLayer.STACK_WALKS.get() - walks0;
    int searchWalks = ErlangMacroLayer.SEARCH_STACK_WALKS.get() - searchWalks0;
    System.out.println("LAYER-AC identifier highlighting: refused=" + skipped + " walks=" + walks + " search walks=" + searchWalks +
                       " builds=" + builds().size());
    assertEquals("no copy build by the identifier highlighting", 0, builds().size());
    assertEquals("no table build by the identifier highlighting", tables0, ErlangMacroLayer.TABLES_BUILT.get());
    assertTrue("it was refused as a passive query", skipped > 1);
    assertEquals("the stack is read once for the questions of the passive calls under one indicator on its thread", 1, walks);
    assertTrue("the layer's searcher reads it at most once per search: " + searchWalks, searchWalks <= 2);
  }

  /** The memo of a passive call is bound to its thread and indicator: another call on the same thread walks again. */
  public void testAPassiveMarkDoesNotOutliveItsIndicator() throws Exception {
    PsiFile s = myFiles.get("s.erl");
    myFixture.configureFromExistingVirtualFile(s.getVirtualFile());
    Editor editor = myFixture.getEditor();
    int caret = s.getText().indexOf("Z + ?CONST");
    PsiElement t = target("s.erl", "Z + ?CONST");
    assertNotNull(t);
    String binding = inBackgroundWithIndicator(() -> where(List.of(t)));
    typeInto("s.erl");
    String[] found = new String[1];
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      ProgressManager.getInstance().runProcess(() -> ReadAction.run(
        () -> new IdentifierHighlightingComputer(s, editor, ProperTextRange.create(0, s.getTextLength()), caret).computeRanges()),
                                               new EmptyProgressIndicator());
      // the same thread, now a resolve (no reference search) under a new indicator: not passive, so it builds
      ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
        PsiReference ref = s.findReferenceAt(caret);
        PsiElement r = ref == null ? null : ref.resolve();
        found[0] = r == null ? "null" : where(List.of(r));
      }), new EmptyProgressIndicator());
    }).get();
    assertEquals(binding, found[0]);
    assertEquals(1, builds().size());
  }
}
