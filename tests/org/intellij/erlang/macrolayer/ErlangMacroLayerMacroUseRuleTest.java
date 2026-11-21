package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingResult;
import com.intellij.codeInsight.daemon.impl.IdentifierOccurrence;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.Segment;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;
import java.util.concurrent.Callable;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * The merge of round 6: the entry points of the bare-use work (references of tokens that only the copy gives a role,
 * the expansion-error pass, completion after {@code ?}) under the build rule of the layer branch, with the quiet time
 * at its default (1 s) and no build on the UI thread. Right after a key press a passive query at the caret (the
 * platform's identifier highlighting) builds nothing and gets master's answer; every other caller builds and gets the
 * layer's answer; a reference search builds the copies of the files just typed in first.
 */
public class ErlangMacroLayerMacroUseRuleTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String RED = """
    -module(red).
    -export([main/0]).

    -define(PAR, ()).

    main() ->
      foo(),
      ?PAR,

      io:format("hello").

    foo() ->
      ok.
    """;

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
    myFile = myFixture.addFileToProject("r/" + name, text);
    myFixture.configureFromExistingVirtualFile(myFile.getVirtualFile());
    myFixture.doHighlighting();
  }

  /** A key press: one character in a comment at the end of the file, committed. Returns System.nanoTime() of it. */
  private long typeAKey() {
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(myFile);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(d.getTextLength(), "%"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    return System.nanoTime();
  }

  private static void assertInsideTheQuietTime(long typedAt, String what) {
    long ms = (System.nanoTime() - typedAt) / 1_000_000;
    System.out.println("MERGED-RULE " + what + " ended " + ms + " ms after the key press");
    assertTrue(what + " must end inside the quiet time: " + ms + " ms", ms < ErlangMacroLayer.quietMillis());
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

  /** The platform's identifier highlighting at the marker, on a pooled thread under a progress indicator, as the IDE runs it. */
  private String identifierHighlighting(String marker) throws Exception {
    Editor editor = myFixture.getEditor();
    int caret = offset(marker);
    PsiFile f = myFile;
    String[] r = new String[1];
    ApplicationManager.getApplication().executeOnPooledThread(() -> ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
      IdentifierHighlightingResult result = new IdentifierHighlightingComputer(f, editor, ProperTextRange.create(0, f.getTextLength()), caret).computeRanges();
      List<String> occurrences = new ArrayList<>();
      for (IdentifierOccurrence o : result.occurrences()) occurrences.add(at(f, o.range()));
      List<String> targets = new ArrayList<>();
      for (Segment s : result.targets()) targets.add(at(f, s));
      Collections.sort(occurrences);
      Collections.sort(targets);
      r[0] = "occurrences=" + occurrences + " targets=" + targets;
    }), new EmptyProgressIndicator())).get();
    return r[0];
  }

  private String targetAt(String marker, boolean withIndicator) throws Exception {
    Editor editor = myFixture.getEditor();
    int caret = offset(marker);
    Callable<String> c = () -> {
      PsiElement t = TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret);
      return t == null ? "null" : at(t.getContainingFile(), t.getTextRange()).replace('\n', ' ');
    };
    return withIndicator ? inBackgroundWithIndicator(c) : inBackgroundWithoutIndicator(c);
  }

  private PsiElement function(String name) throws Exception {
    return inBackgroundWithIndicator(() -> ((org.intellij.erlang.psi.ErlangFile)myFile).getFunction(name, 0));
  }

  private static String usages(PsiElement target) throws Exception {
    return inBackgroundWithIndicator(() -> {
      List<String> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(target).findAll()) {
        PsiElement e = ref.getElement();
        r.add(at(e.getContainingFile(), ref.getRangeInElement().shiftRight(e.getTextRange().getStartOffset())));
      }
      Collections.sort(r);
      return r.toString();
    });
  }

  private static List<String> buildTriggers() {
    List<String> r = new ArrayList<>();
    for (ErlangMacroLayerLog.Event e : builds()) r.add(e.trigger());
    return r;
  }

  private static long refusedAsTyping() {
    return ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals("SKIPPED") && e.line().contains("typing")).count();
  }

  /**
   * {@code foo} in {@code foo?PAR}: only the copy makes it the name of a call of foo/0. The identifier highlighting at
   * it is a passive query: right after a key press it builds nothing and shows master's answer. It is not run again
   * when the daemon has built the copy (in 262 it runs on caret, selection and document events only); its next run,
   * still inside the quiet time, uses the daemon's copy and shows all three foo.
   */
  public void testIdentifierHighlightingOfACopyRoleTokenRightAfterAKeyPress() throws Exception {
    open("main.erl", ErlangMacroUseTest.OWNER);
    int builds0 = builds().size();
    System.out.println("MERGED-RULE builds by the first highlighting: " + buildTriggers());
    String all = "occurrences=[11:foo, 12:foo, 16:foo]";
    String before = identifierHighlighting("f|oo?PAR");
    System.out.println("MERGED-RULE ihc at foo, copy there: " + before);
    assertTrue(before, before.startsWith(all));

    long typed = typeAKey();
    String refused = identifierHighlighting("f|oo?PAR");
    System.out.println("MERGED-RULE ihc at foo right after a key press: " + refused + " refusals=" + refusedAsTyping());
    assertEquals("no build by the identifier highlighting", builds0, builds().size());
    assertTrue("it was refused as a passive query", refusedAsTyping() > 0);
    assertFalse("master's answer: not the copy's three foo", refused.startsWith(all));
    // the bare macro use resolves in the main tree: right after the key press too, without a copy
    String par = identifierHighlighting("foo?P|AR");
    System.out.println("MERGED-RULE ihc at PAR right after a key press: " + par);
    assertTrue(par, par.startsWith("occurrences=[12:PAR, 8:PAR]"));
    assertEquals(builds0, builds().size());

    myFixture.doHighlighting();
    System.out.println("MERGED-RULE builds after the daemon's pass: " + buildTriggers());
    assertEquals("the daemon's highlighting builds the copy", builds0 + 1, builds().size());
    String after = identifierHighlighting("f|oo?PAR");
    assertInsideTheQuietTime(typed, "the next identifier highlighting");
    System.out.println("MERGED-RULE ihc at foo after the daemon's pass: " + after);
    assertEquals(before, after);
    assertEquals("it used the daemon's copy", builds0 + 1, builds().size());
  }

  /**
   * Go to declaration (a caller with an indicator), the rename action's update (a caller without one) and find usages
   * at {@code foo} in {@code foo?PAR}, each right after a key press: they build the copy and get the layer's answer.
   */
  public void testCopyRoleTokenAnswersOtherCallersRightAfterAKeyPress() throws Exception {
    open("main.erl", ErlangMacroUseTest.OWNER);
    String declaration = targetAt("f|oo?PAR", true);
    assertEquals("16:foo() ->   ok.", declaration);

    long typed = typeAKey();
    int builds0 = builds().size();
    assertEquals(declaration, targetAt("f|oo?PAR", true));
    assertInsideTheQuietTime(typed, "go to declaration");
    assertEquals("go to declaration built the copy", builds0 + 1, builds().size());

    typed = typeAKey();
    int own0 = ErlangMacroLayer.OWN_INDICATOR_REQUESTS.get();
    assertEquals(declaration, targetAt("f|oo?PAR", false));
    assertInsideTheQuietTime(typed, "a caller without an indicator");
    assertTrue("it built under the layer's own indicator", ErlangMacroLayer.OWN_INDICATOR_REQUESTS.get() > own0);
    assertEquals(builds0 + 2, builds().size());

    typed = typeAKey();
    PsiElement foo = function("foo");
    assertEquals("[11:foo, 12:foo]", usages(foo));
    assertInsideTheQuietTime(typed, "find usages");
    assertEquals(builds0 + 3, builds().size());
    System.out.println("MERGED-RULE builds: " + buildTriggers());
  }

  /**
   * A find usages of foo/0 whose every question is wrongly taken for a passive one, right after a key press: the
   * layer's reference searcher builds the copy of the file just typed in first, so the use in {@code foo?PAR} is found.
   */
  public void testFindUsagesWronglyTakenForPassiveStillFindsTheCopyRoleToken() throws Exception {
    open("main.erl", ErlangMacroUseTest.OWNER);
    PsiElement foo = function("foo");
    assertEquals("[11:foo, 12:foo]", usages(foo));
    long typed = typeAKey();
    String found = inBackgroundWithIndicator(() -> ErlangMacroLayer.computeAsPassiveQueryForTests(() -> {
      List<String> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(foo).findAll()) {
        PsiElement e = ref.getElement();
        r.add(at(e.getContainingFile(), ref.getRangeInElement().shiftRight(e.getTextRange().getStartOffset())));
      }
      Collections.sort(r);
      return r.toString();
    }));
    assertInsideTheQuietTime(typed, "the wrongly passive find usages");
    assertEquals("[11:foo, 12:foo]", found);
    assertTrue("the searcher built the copy: " + buildTriggers(), buildTriggers().contains("search"));
  }

  /**
   * The expansion-error pass right after a key press, on a background thread under an indicator that is not the
   * daemon's (the worst case: in 262 the platform runs it under a daemon indicator, which is never refused): it is not
   * a passive query, so it builds the copy at once and reports the error without waiting for the quiet time.
   */
  public void testExpansionErrorPassRightAfterAKeyPress() throws Exception {
    open("red.erl", RED);
    long typed = typeAKey();
    int builds0 = builds().size();
    ErlangMacroLayerExpansionErrorAnnotator annotator = new ErlangMacroLayerExpansionErrorAnnotator();
    ErlangMacroLayerExpansionErrorAnnotator.Info info = ReadAction.compute(() -> annotator.collectInformation(myFile));
    assertNotNull(info);
    ErlangMacroLayerExpansionErrorAnnotator.Result result = ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> annotator.doAnnotate(info), new EmptyProgressIndicator())).get();
    assertInsideTheQuietTime(typed, "the expansion-error pass");
    assertNotNull(result);
    List<String> problems = new ArrayList<>();
    for (ErlangMacroLayerExpansionErrors.Problem p : result.problems()) problems.add(at(myFile, p.range()) + " " + p.message());
    assertEquals(List.of("8:?PAR The expansion of ?PAR is not valid Erlang: syntax error before: ')'"), problems);
    assertEquals("the pass built the copy: " + buildTriggers(), builds0 + 1, builds().size());
    assertEquals("expansion-errors", buildTriggers().get(buildTriggers().size() - 1));
    assertEquals("not refused", 0, refusedAsTyping());
  }

  /** Completion of macro names after {@code ?} reads only the main tree: it works right after typing, on the UI thread, without a copy. */
  public void testCompletionAfterAQuestionMarkRightAfterTyping() {
    open("main.erl", ErlangMacroUseTest.OWNER);
    int builds0 = builds().size();
    myFixture.getEditor().getCaretModel().moveToOffset(offset("foo?PAR|,"));
    myFixture.type(",\n  foo?P");
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    myFixture.completeBasic();
    List<String> variants = myFixture.getLookupElementStrings();
    System.out.println("MERGED-RULE completion after foo?P: " + variants + " document: " + myFixture.getEditor().getDocument().getText().contains("foo?PAR,\n  foo?PAR"));
    assertTrue("PAR is offered or inserted: " + variants,
               variants == null ? myFixture.getEditor().getDocument().getText().contains("foo?PAR,\n  foo?PAR") : variants.contains("PAR"));
    assertEquals("completion built no copy", builds0, builds().size());
  }
}
