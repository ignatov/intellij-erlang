package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, fix 1: the gate on "needs substitution". Plain macros (a body that is one value, whose only variables are its
 * parameters, using only plain macros) used in a plain slot need no copy; a form whose uses are all such asks the layer
 * nothing. The soundness check: with the narrow gate and with the wide gate of round 4, the problems and every resolve
 * target are the same.
 */
public class ErlangMacroLayerPlainGateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String FIXTURE = """
    -module(pg).
    -export([f1/1, f2/1, f3/1, g/1, h/2, k/1, m/1, n/1, spec_only/1]).
    -record(rec, {f = 1, g}).
    -define(A, a).
    -define(T, integer()).
    -define(F(X), {X}).
    -define(CALL(X), foo(X)).
    -define(REC(V), #rec{f = V}).
    -define(GET(R), R#rec.f).
    -define(M, lists).
    -define(RN, rec).
    -define(NEST(X), ?F(?A, X)).
    -define(STR(X), ??X).
    -define(MAPPED(K, V), #{K => V}).
    -define(BIND(X), X = 1).
    -define(FREE, Y).
    -define(SEQ, a, b).
    -define(USES_BIND(X), ?BIND(X)).
    -define(USES_UNKNOWN, ?NOT_DEFINED).
    -define(LC(L), [E || E <- L]).
    -ifdef(DEBUG).
    -define(CASE(X, Y), case X of true -> Y; _ -> try Y catch _:_ -> fun(_) -> Y end end end).
    -else.
    -define(CASE(X, Y), ok).
    -endif.
    -define(MATCHES(X, P), case X of P -> true; _ -> false end).
    -define(FUNARG(P), fun(P) -> ok end).
    -spec spec_only(?T) -> ?A.
    spec_only(X) -> X.
    f1(?F(X)) -> X;
    f1(?A) -> ?CALL(?A);
    f1(Y) -> Z = ?F(Y), ?F(W) = Z, {W, ?REC(Y), ?GET(Y), ?M:map(Y, Z), ?STR(Y), ?NEST(Y), ?MAPPED(Y, W), Unbound}.
    f2(Y) -> #?RN{f = Y}, Y#?RN.f, Unbound2.
    f3(Y) -> {fun ?M:foo/1, Y, Unbound3}.
    g(Q) -> ?BIND(R), R + Q, ?FREE, Y, ?USES_BIND(S), S, ?USES_UNKNOWN, ?LC(Q).
    h(A, B) -> case A of ?F(C) -> C; _ -> ?A end, [D || ?F(D) <- B], Unbound4.
    k(X) -> ?SEQ, X.
    m(Q) -> ?CASE(Q, R = 1), {R, ?MATCHES(Q, {S}), S, ?FUNARG(T), T}.
    n(Q) -> ?CASE(Q, R = 1), {R, Unbound5}.
    foo(X) -> X.
    """;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
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

  private ErlangMacroLayer.Table table(PsiFile f) throws Exception {
    return inBackground(() -> ErlangMacroLayer.table((ErlangFile)f));
  }

  public void testClassification() throws Exception {
    PsiFile f = myFixture.addFileToProject("pg.erl", FIXTURE);
    ErlangMacroLayer.Table t = table(f);
    assertEquals(new TreeSet<>(List.of("BIND", "FREE", "SEQ", "USES_BIND", "USES_UNKNOWN", "LC", "MATCHES", "FUNARG")), new TreeSet<>(t.nonPlain));
    assertTrue(t.freeBodyVars.toString(), t.freeBodyVars.containsAll(List.of("Y", "E")));
    assertFalse("a parameter is not a free variable", t.freeBodyVars.contains("X"));
    Map<String, Boolean> needs = inBackground(() -> {
      Map<String, Boolean> m = new TreeMap<>();
      for (PsiElement c = f.getFirstChild(); c != null; c = c.getNextSibling()) {
        String name = c instanceof ErlangFunction fn ? fn.getName() : c.getText().startsWith("-spec") ? "spec" : null;
        if (name != null) m.put(name, ErlangMacroLayer.formNeedsCopy(t, c));
      }
      return m;
    });
    assertEquals("{f1=false, f2=true, f3=true, foo=false, g=true, h=false, k=true, m=true, n=false, spec=false, spec_only=false}", needs.toString());
  }

  public void testHeaderMacrosAreClassified() throws Exception {
    myFixture.addFileToProject("hd/h.hrl", "-define(HP(X), [X]).\n-define(HB(X), X = 2).\n-define(HU, ?HP(1)).\n-define(HV, ?HB(Q)).\n");
    PsiFile f = myFixture.addFileToProject("hd/a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([f/1, g/0]).\n" +
                                                          "f(A) -> {?HP(A), ?HU}.\ng() -> ?HB(Z), ?HV, Z.\n");
    ErlangMacroLayer.Table t = table(f);
    assertEquals(new TreeSet<>(List.of("HB", "HV")), new TreeSet<>(t.nonPlain));
  }

  /** A file whose macros are all plain: highlighting builds no copy and asks the layer nothing. */
  public void testPlainFileAsksNothing() throws Exception {
    PsiFile f = myFixture.addFileToProject("plain.erl", """
      -module(plain).
      -export([f/1]).
      -define(A, a).
      -define(F(X), {X, ?A}).
      -define(G(R), R#r.f).
      -record(r, {f = ?A}).
      -spec f(term()) -> term().
      f(?F(X)) -> Y = ?F(X), ?G(Y), Z = ?A, {Z, undefined_function(W)}.
      """);
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    ErlangMacroLayerStats.reset();
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    resolveAll(f);
    assertEquals("copy builds", 0, builds().size());
    List<String> asked = new ArrayList<>();
    ErlangMacroLayerStats.snapshot().forEach((k, v) -> {
      String[] p = k.split("\t");
      if (p[0].equals("plain.erl") && !p[2].startsWith("gate:") && !p[2].startsWith("nested:gate:") && !p[1].startsWith("table-")) {
        asked.add(k + " x" + v[0]);
      }
    });
    assertEquals("queries beyond the gate", List.of(), asked);
  }

  /** The soundness check on the fixture: problems and resolve targets with the narrow gate equal those of the wide gate. */
  public void testNarrowGateGivesTheAnswersOfTheWideGate() throws Exception {
    PsiFile f = myFixture.addFileToProject("pg.erl", FIXTURE);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    String narrow = answers(f, false), wide = answers(f, true);
    assertEquals(wide, narrow);
    // the fixture exercises the layer: the copy answers for R of g/1 (bound by ?BIND(R))
    assertTrue(narrow, narrow.lines().anyMatch(l -> l.startsWith("R@") && !l.contains("-> null")));
  }

  /** Problems (warnings and above) and every resolve target of the file, with the given gate. */
  String answers(PsiFile f, boolean wideGate) throws Exception {
    Disposable d = Disposer.newDisposable();
    try {
      ErlangMacroLayer.setGateWideForTests(wideGate, d);
      PsiManager.getInstance(getProject()).dropPsiCaches();
      com.intellij.codeInsight.daemon.DaemonCodeAnalyzer.getInstance(getProject()).restart();
      List<String> problems = new ArrayList<>();
      for (HighlightInfo i : myFixture.doHighlighting()) {
        if (i.getSeverity().compareTo(HighlightSeverity.WEAK_WARNING) < 0 || i.getDescription() == null) continue;
        problems.add(i.getStartOffset() + ":" + i.getDescription());
      }
      Collections.sort(problems);
      return String.join("\n", problems) + "\n--\n" + resolveTargets(f);
    }
    finally {
      Disposer.dispose(d);
    }
  }

  static String resolveTargets(PsiFile file) throws Exception {
    return inBackground(() -> {
      StringBuilder sb = new StringBuilder();
      for (PsiElement e : PsiTreeUtil.collectElements(file, x -> x.getReferences().length > 0)) {
        PsiReference[] refs = e.getReferences();
        for (int i = 0; i < refs.length; i++) {
          List<String> targets = new ArrayList<>();
          if (refs[i] instanceof PsiPolyVariantReference p) {
            for (ResolveResult r : p.multiResolve(false)) targets.add(where(r.getElement()));
          }
          PsiElement single = refs[i].resolve();
          sb.append(e.getText(), 0, Math.min(16, e.getTextLength())).append('@').append(e.getTextOffset()).append('#').append(i)
            .append(" -> ").append(where(single)).append(' ').append(targets).append('\n');
        }
      }
      return sb.toString();
    });
  }

  private static String where(PsiElement t) {
    if (t == null) return "null";
    PsiFile f = t.getContainingFile();
    return (f == null ? "?" : f.getName()) + ":" + t.getTextOffset() + ":" + t.getClass().getSimpleName();
  }
}
