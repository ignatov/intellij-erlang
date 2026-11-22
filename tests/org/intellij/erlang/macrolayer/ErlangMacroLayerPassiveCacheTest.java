package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingManager;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingResult;
import com.intellij.codeInsight.daemon.impl.IdentifierOccurrence;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.Segment;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import kotlin.coroutines.EmptyCoroutineContext;
import kotlinx.coroutines.BuildersKt;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6 (next), item 1: the platform keeps the answer of the identifier highlighting at the caret (on range markers
 * of its targets) until the next change of the document or of the PSI of the file. A copy build changes neither, so an
 * answer given while the file is typed in, before the copy is there, would stay. A passive query that the layer would
 * refuse right after a key press therefore waits, cancellably, for the copy (built by the daemon), and gives its answer
 * only then. The tests go through {@code IdentifierHighlightingManager.getMarkupData}, the cached path that the IDE's
 * {@code BackgroundHighlighter} uses, with the quiet time at its default (1 s) and no build on the UI thread.
 */
public class ErlangMacroLayerPassiveCacheTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private PsiFile myFile;

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

  private void open(String name, String text) {
    myFile = myFixture.addFileToProject("p/" + name, text);
    myFixture.configureFromExistingVirtualFile(myFile.getVirtualFile());
    myFixture.doHighlighting();
  }

  /** A key press: one character in a comment at the end of the file, committed. */
  private void typeAKey() {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFile);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  private int offset(String marker) {
    int bar = marker.indexOf('|');
    String m = marker.substring(0, bar) + marker.substring(bar + 1);
    int at = myFile.getText().indexOf(m);
    assertTrue("no " + m, at >= 0);
    return at + bar;
  }

  private static String at(PsiFile f, Segment s) {
    return (StringUtil.offsetToLineNumber(f.getText(), s.getStartOffset()) + 1) + ":" + f.getText().substring(s.getStartOffset(), s.getEndOffset());
  }

  private static String format(PsiFile f, IdentifierHighlightingResult r) {
    List<String> o = new ArrayList<>();
    for (IdentifierOccurrence x : r.occurrences()) o.add(at(f, x.range()));
    Collections.sort(o);
    return o.toString();
  }

  /** The manager's answer at the caret, as BackgroundHighlighter asks for it, on a pooled thread. */
  private Future<String> manager() {
    Editor editor = myFixture.getEditor();
    PsiFile f = myFile;
    IdentifierHighlightingManager m = IdentifierHighlightingManager.Companion.getInstance(getProject());
    return ApplicationManager.getApplication().executeOnPooledThread(() -> {
      IdentifierHighlightingResult r = (IdentifierHighlightingResult)BuildersKt.runBlocking(
        EmptyCoroutineContext.INSTANCE, (scope, cont) -> m.getMarkupData(editor, ProperTextRange.create(0, f.getTextLength()), cont));
      return ReadAction.compute(() -> format(f, r));
    });
  }

  private static long waited() {
    return count("WAITED");
  }

  /**
   * The owner's case: {@code foo} in {@code foo?PAR} is a call of foo/0 only in the copy. Right after a key press the
   * identifier highlighting at it gives its answer only after the daemon has built the copy; the answer that the
   * platform keeps is the copy's (all three foo), also when the caret leaves and comes back.
   */
  public void testIdentifierHighlightingAtACopyOnlyTokenRightAfterAKeyPress() throws Exception {
    open("main.erl", ErlangMacroUseTest.OWNER);
    String all = "[11:foo, 12:foo, 16:foo]";
    myFixture.getEditor().getCaretModel().moveToOffset(offset("f|oo?PAR"));
    assertEquals("the copy is there", all, manager().get(30, TimeUnit.SECONDS));
    int builds0 = builds().size();

    typeAKey();
    Future<String> rightAfter = manager();
    Thread.sleep(200); // the identifier highlighting runs before the daemon
    assertEquals("the identifier highlighting built nothing", builds0, builds().size());
    myFixture.doHighlighting();
    assertEquals("the daemon built the copy", builds0 + 1, builds().size());
    String given = rightAfter.get(30, TimeUnit.SECONDS);
    assertEquals("the answer the platform keeps", all, manager().get(30, TimeUnit.SECONDS));
    assertEquals("the answer given right after the key press", all, given);
    myFixture.getEditor().getCaretModel().moveToOffset(offset("io:f|ormat"));
    manager().get(30, TimeUnit.SECONDS);
    myFixture.getEditor().getCaretModel().moveToOffset(offset("f|oo?PAR"));
    assertEquals("caret away and back", all, manager().get(30, TimeUnit.SECONDS));
    assertEquals(builds0 + 1, builds().size());
  }

  /** The same for a variable that only a macro body binds ({@code ?BIND(Z)}): the use and the binding. */
  public void testIdentifierHighlightingAtAVariableBoundByAMacroRightAfterAKeyPress() throws Exception {
    open("w.erl", "-module(w).\n-define(BIND(V), V = 1).\n-export([f/0]).\nf() -> ?BIND(Z), Z.\n");
    String both = "[4:Z, 4:Z]";
    myFixture.getEditor().getCaretModel().moveToOffset(offset("|Z."));
    assertEquals("the copy is there", both, manager().get(30, TimeUnit.SECONDS));
    int builds0 = builds().size();

    typeAKey();
    Future<String> rightAfter = manager();
    Thread.sleep(200);
    assertEquals(builds0, builds().size());
    myFixture.doHighlighting();
    assertEquals(builds0 + 1, builds().size());
    String given = rightAfter.get(30, TimeUnit.SECONDS);
    assertEquals("the answer the platform keeps", both, manager().get(30, TimeUnit.SECONDS));
    assertEquals(both, given);
  }

  /**
   * A file whose macros are all plain (no form needs the copy): the identifier highlighting right after a key press
   * answers at once, without waiting, and the answer is master's, which is the layer's here.
   */
  public void testNoWaitInAFileWhoseFormsNeedNoCopy() throws Exception {
    open("plain.erl", "-module(plain).\n-define(A, 1).\n-export([f/1]).\nf(X) -> Y = X + ?A, Y.\n");
    myFixture.getEditor().getCaretModel().moveToOffset(offset("|Y."));
    String before = manager().get(30, TimeUnit.SECONDS);
    assertEquals("[4:Y, 4:Y]", before);
    typeAKey();
    long t0 = System.nanoTime();
    assertEquals(before, manager().get(30, TimeUnit.SECONDS));
    long ms = (System.nanoTime() - t0) / 1_000_000;
    System.out.println("PASSIVE-CACHE plain file answered in " + ms + " ms");
    assertEquals("no wait", 0, waited());
    assertEquals("no build", 0, builds().size());
  }

  /**
   * While the user goes on typing, every key press cancels the waiting query (a write action cancels the read action
   * of the identifier highlighting); it never starts a build. The daemon's build after the last key serves it.
   */
  public void testKeyPressesCancelTheWaitAndNothingIsBuiltWhileTyping() throws Exception {
    open("main.erl", ErlangMacroUseTest.OWNER);
    Editor editor = myFixture.getEditor();
    editor.getCaretModel().moveToOffset(offset("f|oo?PAR"));
    int builds0 = builds().size();
    int caret = offset("f|oo?PAR");
    PsiFile f = myFile;
    List<Future<?>> queries = new ArrayList<>();
    for (int i = 0; i < 5; i++) {
      typeAKey();
      // the identifier highlighting after the key, in a non-blocking read action as the IDE runs it
      queries.add(ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.nonBlocking(
        () -> format(f, new IdentifierHighlightingComputer(f, editor, ProperTextRange.create(0, f.getTextLength()), caret).computeRanges()))
        .executeSynchronously()));
      Thread.sleep(80);
    }
    assertEquals("no build while typing", builds0, builds().size());
    assertEquals("no build cancelled", 0, count("CANCELLED"));
    assertTrue("the queries waited", waited() > 0);
    myFixture.doHighlighting();
    for (Future<?> q : queries) assertEquals("[11:foo, 12:foo, 16:foo]", q.get(30, TimeUnit.SECONDS));
    assertEquals("only the daemon built", builds0 + 1, builds().size());
    assertEquals(0, count("CANCELLED"));
  }
}
