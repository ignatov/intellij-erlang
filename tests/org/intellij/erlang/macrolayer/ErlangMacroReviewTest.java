package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.util.registry.Registry;
import com.intellij.psi.*;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Remarks of the code review of the macro references, resolve, find usages and rename (C1-C11), and the fuzz finding
 * F7. Each expectation follows epp (erlc 29.1): the definition in force at a use, macro names as directives see them.
 */
public class ErlangMacroReviewTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    // A2 switches the layer through the Registry key, the user's way
    if (!getName().startsWith("testA2")) ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private int myCount;

  private PsiFile open(String text) {
    PsiFile f = myFixture.addFileToProject("r" + myCount++ + "/m.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    return f;
  }

  private void caretAt(String marker) {
    String text = myFixture.getFile().getText();
    myFixture.getEditor().getCaretModel().moveToOffset(text.indexOf(marker.replace("|", "")) + marker.indexOf('|'));
  }

  private static List<String> targets(PsiFile f, String marker) throws Exception {
    int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      List<String> r = new ArrayList<>();
      if (ref == null) return r;
      List<PsiReference> refs = ref instanceof com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference m ? List.of(m.getReferences()) : List.of(ref);
      for (PsiReference one : refs) {
        if (!(one.resolve() instanceof ErlangMacrosDefinition) && !(one instanceof PsiPolyVariantReference)) continue;
        if (one instanceof PsiPolyVariantReference p) {
          for (ResolveResult rr : p.multiResolve(false)) if (rr.getElement() instanceof ErlangMacrosDefinition d) r.add(d.getText());
        }
        else r.add(one.resolve().getText());
      }
      return new ArrayList<>(new java.util.LinkedHashSet<>(r));
    });
  }

  // C1: rename from a use of a macro defined in both branches of -ifdef
  public void testC1RenameFromAUseOfAMacroInBothBranches() {
    open("-module(m).\n-export([f/0]).\n-ifdef(TEST).\n-define(LOG(X), {t, X}).\n-else.\n-define(LOG(X), {p, X}).\n-endif.\nf() -> ?LOG(1).\n");
    caretAt("?L|OG(1)");
    myFixture.renameElementAtCaret("TRACE");
    assertEquals("-module(m).\n-export([f/0]).\n-ifdef(TEST).\n-define(TRACE(X), {t, X}).\n-else.\n-define(TRACE(X), {p, X}).\n-endif.\nf() -> ?TRACE(1).\n",
                 myFixture.getFile().getText());
  }

  // C1, layer off: ?A has two references in master's tree (the use and its name)
  public void testC1RenameFromAUseWithTheLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    open("-module(m).\n-export([f/0]).\n-define(A, 1).\nf() -> ?A.\n");
    caretAt("?|A.");
    myFixture.renameElementAtCaret("B");
    assertEquals("-module(m).\n-export([f/0]).\n-define(B, 1).\nf() -> ?B.\n", myFixture.getFile().getText());
  }

  // C4: the definition in force at the use
  public void testC4DefinitionInForceAtTheUse() throws Exception {
    PsiFile f = open("-module(m).\n-export([f/0, g/0]).\n-define(V, 1).\nf() -> ?V.\n-undef(V).\n-define(V, 2).\ng() -> ?V.\n");
    assertEquals(List.of("-define(V, 1)."), targets(f, "f() -> ?|V"));
    assertEquals(List.of("-define(V, 2)."), targets(f, "g() -> ?|V"));
    ErlangMacrosDefinition first = ((ErlangFile)f).getMacroses().get(0);
    List<Integer> uses = inBackgroundWithIndicator(() -> ReferencesSearch.search(first).findAll().stream()
      .map(r -> r.getElement().getTextRange().getStartOffset() + r.getRangeInElement().getStartOffset()).toList());
    // f's use and the -undef that ends the definition; not g's use
    assertEquals(uses.toString(), 2, uses.size());
    for (int u : uses) assertTrue("no usage after the second definition: " + uses, u < f.getText().indexOf("-define(V, 2)"));
  }

  // C2: renaming a macro leaves a header's definition of the same name alone (it is not an alternative of it)
  public void testC2RenameDoesNotTouchAHeaderDefinitionOfTheSameName() {
    PsiFile h = myFixture.addFileToProject("r2/h.hrl", "-define(EQ(A, B), A =:= B).\n");
    myFixture.addFileToProject("r2/m.erl", "-module(m).\n-include(\"h.hrl\").\n-export([f/0]).\n-undef(EQ).\n-define(EQ(A, B), my:eq(A, B)).\nf() -> ?EQ(1, 2).\n");
    myFixture.configureFromExistingVirtualFile(myFixture.findFileInTempDir("r2/m.erl"));
    caretAt("define(E|Q(A, B), my");
    myFixture.renameElementAtCaret("SAME");
    assertEquals("-define(EQ(A, B), A =:= B).\n", h.getText());
    assertTrue(myFixture.getFile().getText(), myFixture.getFile().getText().contains("-define(SAME(A, B), my:eq(A, B)).\nf() -> ?SAME(1, 2)."));
  }

  // C3: a directive names every arity; renaming one arity renames the name everywhere in the file, never half of it
  public void testC3RenameOfOneArityWithADirective() {
    open("-module(m).\n-export([f/0, g/0]).\n-define(M(), z).\n-define(M(X), {X}).\nf() -> ?M(1).\n-undef(M).\n-define(M(), two).\ng() -> ?M().\n");
    caretAt("?|M(1)");
    myFixture.renameElementAtCaret("N");
    assertEquals("-module(m).\n-export([f/0, g/0]).\n-define(N(), z).\n-define(N(X), {X}).\nf() -> ?N(1).\n-undef(N).\n-define(N(), two).\ng() -> ?N().\n",
                 myFixture.getFile().getText());
  }

  // A2: when the Registry key goes off, the kept copies are let go at once
  public void testA2SwitchOffReleasesKeptCopies() throws Exception {
    PsiFile f = open("-module(m).\n-export([f/0]).\n-define(CALL(M, F), M:F(), ok).\nf() -> ?CALL(b, g).\n");
    assertNotNull(inBackgroundWithIndicator(() -> ErlangMacroLayer.getCopy((ErlangFile)f, "test")));
    ErlangMacroLayerRetention retention = ErlangMacroLayerRetention.getInstance(getProject());
    assertTrue(retention.strongCount() > 0);
    Registry.get(ErlangMacroLayer.REGISTRY_KEY).setValue(false, getTestRootDisposable());
    assertEquals(0, retention.strongCount());
  }

  // C6: a bare use inside a conditional on the same name is not "unresolved" (the name comes from -D)
  public void testC6BareUseInsideItsOwnConditionalIsNotUnresolved() {
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnresolvedMacrosInspection.class);
    open("-module(m).\n-export([f/0]).\n-ifdef(mac).\nf() -> foo?mac(a).\n-endif.\nfoo(X) -> X.\n");
    for (HighlightInfo h : myFixture.doHighlighting()) {
      assertFalse(h.getDescription(), String.valueOf(h.getDescription()).startsWith("Unresolved macros"));
    }
  }

  // C9: a rename to a name that needs quotes renames every use, or nothing; never half
  public void testC9RenameToAQuotedNameIsNeverHalfDone() {
    String text = "-module(m).\n-export([f/0, foo/0]).\n-define(PAR, ()).\nf() -> foo?PAR.\nfoo() -> ok.\n";
    open(text);
    caretAt("define(P|AR");
    try {
      myFixture.renameElementAtCaret("p q");
    }
    catch (RuntimeException e) {
      // a refusal before any change is fine
      assertEquals(text, myFixture.getFile().getText());
      return;
    }
    assertEquals("-module(m).\n-export([f/0, foo/0]).\n-define('p q', ()).\nf() -> foo?'p q'.\nfoo() -> ok.\n", myFixture.getFile().getText());
  }

  // C11: a header guard's name goes to the definition in the conditional body (none exists before it)
  public void testC11HeaderGuardGoesToItsDefinition() throws Exception {
    PsiFile f = open("-module(m).\n-ifndef(GUARD).\n-define(GUARD, 1).\n-endif.\n");
    assertEquals(List.of("-define(GUARD, 1)."), targets(f, "-ifndef(G|UARD"));
  }

  // F7: rename of a function whose first clause a fragment macro makes, from its name in -export
  public void testF7RenameFromExportOfAFunctionWithAClauseMadeByAMacro() {
    open("-module(m).\n-export([sign/1]).\n-define(HEAD(N), sign(N) when N > 0 ->).\n?HEAD(X) positive;\nsign(0) -> zero;\nsign(_) -> negative.\n");
    myFixture.doHighlighting();
    caretAt("-export([s|ign/1])");
    myFixture.renameElementAtCaret("renamed_sign");
    assertEquals("-module(m).\n-export([renamed_sign/1]).\n-define(HEAD(N), renamed_sign(N) when N > 0 ->).\n?HEAD(X) positive;\nrenamed_sign(0) -> zero;\nrenamed_sign(_) -> negative.\n",
                 myFixture.getFile().getText());
  }

  // A1: the platform's copy of a file (as completion makes) is not a layer file: its caches would answer for old text
  public void testA1PlatformCopyOfAFileIsNotALayerFile() throws Exception {
    PsiFile f = open("-module(m).\n-export([f/0]).\n-define(CALL(M, F), M:F(), ok).\nf() -> ?CALL(b, g).\n");
    PsiFile copy = (PsiFile)f.copy();
    assertNotNull(inBackgroundWithIndicator(() -> ErlangMacroLayer.layerFile(f)));
    assertNull(inBackgroundWithIndicator(() -> ErlangMacroLayer.layerFile(copy)));
  }

  // A3: after a header changes, a UI-thread caller does not read and lex the header
  public void testA3NoHeaderSummaryOnTheUiThread() throws Exception {
    PsiFile h = myFixture.addFileToProject("r3/h.hrl", "-define(CALL(M, F), M:F(), ok).\n");
    PsiFile f = myFixture.addFileToProject("r3/m.erl", "-module(m).\n-include(\"h.hrl\").\n-export([f/0]).\nf() -> ?CALL(b, g).\n");
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    assertNotNull(inBackgroundWithIndicator(() -> ErlangMacroLayer.table((ErlangFile)f)));
    com.intellij.openapi.editor.Document d = PsiDocumentManager.getInstance(getProject()).getDocument(h);
    com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(0, "%% changed\n"));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    int before = ErlangMacroLayer.SUMMARIES_BUILT.get();
    ErlangMacroLayer.table((ErlangFile)f);
    assertEquals("no summary built on the UI thread", before, ErlangMacroLayer.SUMMARIES_BUILT.get());
  }

  private List<String> errorsOf(String text) {
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnresolvedMacrosInspection.class);
    open(text);
    List<String> r = new ArrayList<>();
    for (HighlightInfo h : myFixture.doHighlighting()) {
      if (h.getSeverity() == com.intellij.lang.annotation.HighlightSeverity.ERROR) r.add(h.getDescription());
    }
    return r;
  }

  // D3: no argument mismatch inside a branch that may be skipped (review probe dead_branch.erl, erlc compiles it)
  public void testD3NoArgumentMismatchInAConditionalBranch() {
    assertEquals(List.of(), errorsOf("-module(m).\n-export([f/0]).\n-define(LOG(X), {log, X}).\n-ifdef(LEGACY_LOGGING).\ng() -> ?LOG(a, b).\n-endif.\nf() -> ?LOG(a).\n"));
  }

  // D3: no argument mismatch for a name a conditional tests (it may come from -D; probe dflag.erl with -DLOG=mylog)
  public void testD3NoArgumentMismatchForANameTestedByAConditional() {
    assertEquals(List.of(), errorsOf("-module(m).\n-export([f/0]).\n-ifndef(LOG).\n-define(LOG(Fmt, Args), io:format(Fmt, Args)).\n-endif.\nf() -> ?LOG(\"x~n\").\nmylog(_) -> ok.\n"));
  }

  // D3: the check still works where it is certain
  public void testD3ArgumentMismatchStaysWhereCertain() {
    assertEquals(List.of("Argument mismatch for macro '?LOG': no definition takes 2 arguments"),
                 errorsOf("-module(m).\n-export([f/0]).\n-define(LOG(X), {log, X}).\nf() -> ?LOG(a, b).\n"));
  }

  // D6: two functions of the same name and different arity both stay among the items after a lost position
  public void testD6CompletionKeepsFunctionsOfBothArities() {
    open("-module(m).\n-export([f/0, bar/0, bar/1]).\n-define(PAR, ()).\nf() ->\n  foo?PAR,\n  ba.\nfoo() -> ok.\nbar() -> ok.\nbar(X) -> X.\n");
    myFixture.doHighlighting();
    caretAt("  ba|.");
    com.intellij.codeInsight.lookup.LookupElement[] items = myFixture.complete(com.intellij.codeInsight.completion.CompletionType.BASIC);
    assertNotNull("a list, not one item inserted", items);
    long bars = java.util.Arrays.stream(items).filter(e -> e.getLookupString().equals("bar")).count();
    assertEquals(java.util.Arrays.stream(items).map(e -> e.getLookupString()).toList().toString(), 2, bars);
  }

  // A4 / T5: the search in a header's macro body: one word search per change, at most the retention's number of copies
  public void testA4HeaderBodySearchIsBounded() throws Exception {
    myFixture.addFileToProject("r4/h.hrl", "-define(CALL(), target()).\n");
    List<PsiFile> users = new ArrayList<>();
    for (int i = 0; i < 30; i++) {
      users.add(myFixture.addFileToProject("r4/u" + i + ".erl", "-module(u" + i + ").\n-include(\"h.hrl\").\n-export([f/0, target/0]).\n" +
                                                                 "f() -> ?CALL().\ntarget() -> ok.\n"));
    }
    org.intellij.erlang.psi.ErlangFunction target = inBackgroundWithIndicator(() -> ((ErlangFile)users.get(0)).getFunction("target", 0));
    int searches0 = ErlangMacroLayer.INCLUDER_SEARCHES.get(), builds0 = ErlangMacroLayer.EXPANSIONS_BUILT.get();
    inBackgroundWithIndicator(() -> ReferencesSearch.search(target).findAll().size());
    int searches = ErlangMacroLayer.INCLUDER_SEARCHES.get() - searches0, builds = ErlangMacroLayer.EXPANSIONS_BUILT.get() - builds0;
    assertTrue("word searches for the includers: " + searches, searches <= 1);
    assertTrue("copies built: " + builds, builds <= ErlangMacroLayerRetention.STRONG + 1);
  }

  // T5: a file larger than the copy cap gets master's behaviour
  public void testT5FileOverTheCopyCapIsRefused() throws Exception {
    ErlangMacroLayer.setMaxCopyCharsForTests(200, getTestRootDisposable());
    PsiFile f = open("-module(m).\n-export([f/0]).\n-define(CALL(M, F), M:F(), ok).\n%% " + "x".repeat(300) + "\nf() -> ?CALL(b, g).\n");
    assertEquals("too-large", inBackgroundWithIndicator(() -> ErlangMacroLayer.refusalForTests(f)));
  }

  // master #1436 merged: a variable that master resolves (here matched again many times) never asks the layer
  public void testVariablesMasterResolvesDoNotAskTheLayer() throws Exception {
    StringBuilder body = new StringBuilder("f(X) ->\n  A = X,\n");
    for (int i = 0; i < 30; i++) body.append("  A = X,\n");
    PsiFile f = open("-module(m).\n-export([f/1, g/0]).\n-define(CALL(M, F), M:F(), ok).\n" + body + "  A.\ng() -> ?CALL(b, g).\n");
    int asked = ErlangMacroLayer.FALLBACK_ASKED.get();
    int resolved = inBackgroundWithIndicator(() -> {
      int n = 0;
      for (org.intellij.erlang.psi.ErlangQVar v : com.intellij.psi.util.PsiTreeUtil.findChildrenOfType(f, org.intellij.erlang.psi.ErlangQVar.class)) {
        PsiReference r = v.getReference();
        if (r instanceof PsiPolyVariantReference p && p.multiResolve(false).length > 0) n++;
      }
      return n;
    });
    assertTrue("variables resolved: " + resolved, resolved > 30);
    assertEquals("the layer was not asked", asked, ErlangMacroLayer.FALLBACK_ASKED.get());
  }
}
