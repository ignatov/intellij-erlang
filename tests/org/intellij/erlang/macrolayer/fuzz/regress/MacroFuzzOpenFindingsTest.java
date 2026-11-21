package org.intellij.erlang.macrolayer.fuzz.regress;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.macrolayer.fuzz.EditorActionProbe;
import org.intellij.erlang.macrolayer.fuzz.FuzzConfig;
import org.intellij.erlang.macrolayer.fuzz.LayerHooks;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * Regression tests of findings of the macro fuzz suite that are still open. They run only with
 * {@code MACRO_FUZZ_OPEN=1} (each method passes at once without it), so that the normal test task stays green until the
 * fix lands; the builder moves a method into {@link MacroFuzzRegressionTest} (or next to the fix) when it passes. Each
 * method also passes on master, where the layer does not exist: that is the behaviour asked for with the layer off.
 * <p>
 * Run: {@code MACRO_FUZZ_OPEN=1 ./gradlew --no-daemon test --rerun --tests 'org.intellij.erlang.macrolayer.fuzz.regress.MacroFuzzOpenFindingsTest'}
 */
public class MacroFuzzOpenFindingsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static boolean open() {
    return FuzzConfig.env("MACRO_FUZZ_OPEN") != null;
  }

  private void layerOff() {
    LayerHooks.setEnabled(false, getTestRootDisposable());
  }

  private List<String> referenceClasses() {
    List<String> r = new ArrayList<>();
    for (PsiElement e : PsiTreeUtil.collectElements(myFixture.getFile(), x -> true)) {
      for (PsiReference ref : e.getReferences()) r.add(ref.getClass().getSimpleName() + "@" + (e.getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset()));
    }
    return r;
  }

  /**
   * F4/D1 (wrong answer, branch only; open): with the layer off the branch still hosts bare macro references
   * ({@code ErlangBareMacroReference}) that master does not have. Input: sample {@code src/main.erl} line 22
   * {@code foo?PAR,} with {@code -define(PAR, ()).}. Found by {@code MacroFuzzAnswersTest} with {@code MACRO_FUZZ_LAYER=off}
   * compared with master (seed 20261121, case answers:55).
   */
  public void testF4D1NoBareMacroReferenceWithLayerOff() {
    if (!open()) return;
    layerOff();
    myFixture.configureByText("m.erl", "-module(m).\n-export([f/0]).\n-define(PAR, ()).\nf() ->\n  foo?PAR,\n  foo?PAR.\nfoo() -> ok.\n");
    for (String c : referenceClasses()) assertFalse(c, c.startsWith("ErlangBareMacroReference"));
  }

  /**
   * F4/D4 (wrong answer, branch only; open): with the layer off the branch reports "Unresolved macros '?NO_ARGS'" on a
   * bare use after an atom (sample {@code src/errors/err_undefined_fragment.erl} line 11 {@code foo?NO_ARGS.}); master
   * does not. Case answers:27.
   */
  public void testF4D4NoUnresolvedMacroOnBareUseWithLayerOff() {
    if (!open()) return;
    layerOff();
    EditorActionProbe.enableInspections(myFixture);
    myFixture.configureByText("m.erl", "-module(m).\n-export([f/0]).\nf() ->\n  foo?NO_ARGS.\n");
    for (HighlightInfo h : myFixture.doHighlighting()) {
      assertFalse(h.getDescription(), String.valueOf(h.getDescription()).startsWith("Unresolved macros"));
    }
  }

  /**
   * F4/D4 (wrong answer, branch only; open): with the layer off the branch reports "Argument mismatch for macro '?ID': no
   * definition takes 2 arguments" (sample {@code src/errors/err_arity.erl} line 13 {@code ?ID(1, 2).}); master does not.
   * Case answers:9.
   */
  public void testF4D4NoArgumentMismatchWithLayerOff() {
    if (!open()) return;
    layerOff();
    EditorActionProbe.enableInspections(myFixture);
    myFixture.configureByText("m.erl", "-module(m).\n-export([f/0]).\n-define(ID(X), X).\nf() ->\n  ?ID(1, 2).\n");
    for (HighlightInfo h : myFixture.doHighlighting()) {
      assertFalse(h.getDescription(), String.valueOf(h.getDescription()).startsWith("Argument mismatch"));
    }
  }

  /**
   * F4/D5 (wrong answer, branch only; open): with the layer off, basic completion inside a macro name ({@code ?L|INE})
   * in a function that the main tree loses (a list tail fragment used before its {@code -define}) offers macro names;
   * master offers none. Generated case answers:0, offset 114 (seed 20261121).
   */
  public void testF4D5CompletionInBrokenFunctionAsMasterWithLayerOff() {
    if (!open()) return;
    layerOff();
    myFixture.addFileToProject("h1.hrl", "%% generated header\n-define(H0, 1 + 1).\n");
    myFixture.configureByText("m.erl", "-module(m).\n-include(\"h1.hrl\").\n-define(M4(Name), Name(X) ->).\nf7() ->\n    X = 1,\n" +
                                       "    [1 ?M1(2),\n    ?H0(),\n    ?L<caret>INE,\n    ?MODULE:module_info().\n" +
                                       "-define(M1(X), , X]).\n");
    myFixture.completeBasic();
    List<String> strings = myFixture.getLookupElementStrings();
    assertTrue(String.valueOf(strings), strings == null || strings.isEmpty());
  }

  /**
   * F5 (wrong answer, branch only; open): the macro name in {@code -ifndef(M5).} resolves to the {@code -define(M5, 0).}
   * inside the skipped branch that follows it; epp uses the definitions before it ({@code M5/1}, {@code M5/2}), which
   * is master's answer. Generated case answers:68, offset 514 (seed 20261121), checked with
   * {@code scripts/epp_define_at.py}. Seen with the layer off; the test asks with the layer on and off.
   */
  public void testF5IfndefNameResolvesToDefinitionsBeforeIt() {
    if (!open()) return;
    for (boolean on : new boolean[]{true, false}) {
      com.intellij.openapi.Disposable d = com.intellij.openapi.util.Disposer.newDisposable();
      try {
        LayerHooks.setEnabled(on, d);
        myFixture.configureByText("m" + on + ".erl", "-module(m" + on + ").\n-define(M5(X), {X, ?MODULE}).\n-define(M5(X, Y), [X | Y]).\n" +
                                                     "-ifndef(M<caret>5).\n-define(M5, 0).\n-endif.\nf() -> ?M5(1).\n");
        PsiReference ref = myFixture.getFile().findReferenceAt(myFixture.getCaretOffset());
        assertNotNull(ref);
        String text = myFixture.getFile().getText();
        int skipped = text.indexOf("-define(M5, 0)");
        List<PsiElement> targets = new ArrayList<>();
        if (ref instanceof PsiPolyVariantReference pv) {
          for (ResolveResult rr : pv.multiResolve(false)) if (rr.getElement() != null) targets.add(rr.getElement());
        }
        else if (ref.resolve() != null) targets.add(ref.resolve());
        assertFalse("layer " + on + ": no target", targets.isEmpty());
        for (PsiElement t : targets) {
          assertTrue("layer " + on + ": target in the skipped branch at " + t.getTextOffset(), t.getTextOffset() < skipped);
        }
      }
      finally {
        com.intellij.openapi.util.Disposer.dispose(d);
      }
    }
  }

  /**
   * F6 (wrong answer, branch only; open; the T1 shape of the architecture review): after an edit of ANOTHER file, a layer
   * reference in a macro argument ({@code ErlangMacroLayerTokenReference} at {@code g} in {@code ?M1(b, g)}) resolves to
   * an invalid element: the kept answer still points into the old tree of b.erl. Found by
   * {@code MacroFuzzTypingTest} (seed 201, cases typing:387 and typing:1655): edit b.erl, highlight m.erl, resolve on the
   * UI thread. The same with b.erl's function deleted and with the edit not committed before the highlighting.
   */
  public void testF6NoInvalidTargetAfterEditOfOtherFile() {
    if (!open()) return;
    for (String change : new String[]{"rename", "delete", "touch"}) {
      PsiFile b = myFixture.addFileToProject("f6" + change + "/b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
      myFixture.configureByText("m.erl", "-module(m).\n-export([f3/1]).\n-define(M1(M, F), M:F(), ok).\nf3(X) ->\n    ?M1(b, g),\n    X.\n");
      myFixture.doHighlighting();
      allTargets(true);
      com.intellij.openapi.editor.Document d = PsiDocumentManager.getInstance(getProject()).getDocument(b);
      String text = change.equals("rename") ? "-module(b).\n-export([g_renamed/0]).\ng_renamed() -> ok.\n"
                    : change.equals("delete") ? "-module(b).\n-export([g/0]).\n" : "-module(b).\n-export([g/0]).\n\ng() -> ok.\n";
      com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(text));
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      myFixture.doHighlighting();
      allTargets(true);
      com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> {
        try {
          b.getVirtualFile().getParent().delete(this);
        }
        catch (java.io.IOException e) {
          throw new RuntimeException(e);
        }
      });
    }
  }

  /** Every target of every reference of the open file is valid. */
  private void allTargets(boolean check) {
    for (PsiElement e : PsiTreeUtil.collectElements(myFixture.getFile(), x -> true)) {
      for (PsiReference ref : e.getReferences()) {
        List<PsiElement> ts = new ArrayList<>();
        if (ref instanceof PsiPolyVariantReference pv) {
          for (ResolveResult rr : pv.multiResolve(false)) if (rr.getElement() != null) ts.add(rr.getElement());
        }
        else if (ref.resolve() != null) ts.add(ref.resolve());
        for (PsiElement t : ts) {
          if (check) assertTrue(ref.getClass().getSimpleName() + " at " + e.getTextOffset() + " -> invalid " + t.getClass().getSimpleName(), t.isValid());
        }
      }
    }
  }
}
