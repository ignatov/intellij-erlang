package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Spike 5, task 2: does a file have a macro use that needs substitution, and what does the gate skip for it?
 * For each file: parse errors of the main tree, forms and their expansion status, highlighting problems with the
 * layer off and on, every reference resolved with the layer off and on, and the layer's counters by outcome.
 * Runs only when {@code S5_AUDIT} is set (a comma-separated list of OTP paths relative to the OTP lib directory,
 * or "default").
 */
public class ErlangMacroLayerGateAuditTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib";
  private Disposable myMode;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
  }

  public ErlangMacroLayerGateAuditTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    // the installed OTP 29.1 as the SDK, as in the IDE run
    return new com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor() {
      @Override
      public com.intellij.openapi.projectRoots.Sdk getSdk() {
        return org.intellij.erlang.sdk.ErlangSdkType.createMockSdk("/opt/homebrew/Cellar/erlang/29.1/lib/erlang",
                                                                   org.intellij.erlang.sdk.ErlangSdkRelease.V_29_0);
      }
    };
  }

  private static void out(String s) {
    System.out.println("S5-AUDIT " + s);
  }

  private void mode(ErlangMacroLayer.Mode m) {
    if (myMode != null) Disposer.dispose(myMode);
    myMode = Disposer.newDisposable();
    ErlangMacroLayer.setModeForTests(m, myMode);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myMode != null) Disposer.dispose(myMode);
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  /** Copies the app's src and include directories (and kernel/stdlib includes) so that includes resolve. */
  private void copyApp(Path app, String name, boolean full) throws Exception {
    for (String dir : full ? List.of("src", "include") : List.of("include")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).toList()) {
          String fn = p.getFileName().toString();
          if (fn.endsWith(".hrl") || full && fn.endsWith(".erl")) myFixture.addFileToProject("apps/" + name + "/" + app.relativize(p), read(p));
        }
      }
    }
    myFixture.addFileToProject("apps/" + name + "/ebin/" + name + ".app", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
    myFixture.addFileToProject("apps/" + name + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
  }

  /** Highlighting from scratch: drop the PSI and resolve caches and restart the daemon, so no pass is skipped. */
  private List<HighlightInfo> freshHighlighting() {
    PsiManager.getInstance(getProject()).dropPsiCaches();
    com.intellij.codeInsight.daemon.DaemonCodeAnalyzer.getInstance(getProject()).restart();
    return myFixture.doHighlighting();
  }

  private static List<String> problems(List<HighlightInfo> infos) {
    List<String> r = new ArrayList<>();
    for (HighlightInfo i : infos) {
      if (i.getSeverity().compareTo(HighlightSeverity.WEAK_WARNING) < 0 || i.getDescription() == null) continue;
      r.add(i.getSeverity().getName() + "|" + i.getStartOffset() + "|" + i.getDescription());
    }
    Collections.sort(r);
    return r;
  }

  public void testAudit() throws Exception {
    String list = System.getenv("S5_AUDIT");
    if (list == null) return;
    if (list.equals("default")) list = "dialyzer-6.0.3/src/erl_types.erl,xmerl-2.2/src/xmerl_scan.erl";
    setUpProjectSdk();
    enableAll(myFixture);
    copyApp(Paths.get(OTP, "kernel-11.0.4"), "kernel", false);
    copyApp(Paths.get(OTP, "stdlib-8.1"), "stdlib", false);
    Set<String> apps = new HashSet<>();
    for (String rel : list.split(",")) {
      String app = rel.substring(0, rel.indexOf('/'));
      String name = app.replaceAll("-[0-9.]+$", "");
      if (apps.add(name)) copyApp(Paths.get(OTP, app), name, true);
      String path = "apps/" + name + "/" + rel.substring(rel.indexOf('/') + 1);
      PsiFile file = PsiManager.getInstance(getProject()).findFile(myFixture.findFileInTempDir(path));
      audit(file);
    }
  }

  private void audit(PsiFile file) throws Exception {
    String n = file.getName();
    // the main tree
    int parseErrors = PsiTreeUtil.collectElementsOfType(file, PsiErrorElement.class).size();
    int forms = 0;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) if (!(c instanceof PsiWhiteSpace) && !(c instanceof PsiComment)) forms++;
    out(n + " main tree: top-level elements=" + forms + " parse errors=" + parseErrors + " refusal=" + ErlangMacroLayer.refusalForTests(file));
    // the copy
    mode(ErlangMacroLayer.Mode.LAYER);
    ErlangMacroLayer.Copy c = inBackground(() -> ErlangMacroLayer.build(file));
    Map<String, Integer> st = new TreeMap<>();
    int broken = 0, allFragments = 0, someComplete = 0, uses = 0;
    for (ErlangMacroExpander.Form f : c.expansion.forms) {
      st.merge(f.status.name(), 1, Integer::sum);
      if (f.status == ErlangMacroExpander.FormStatus.EXPANDED) {
        uses += f.substitutedUses;
        if (c.isBroken(f)) broken++;
        if (f.allFragments) allFragments++;
        else someComplete++;
      }
    }
    int copyErrors = c.copyFile == null ? 0 : PsiTreeUtil.collectElementsOfType(c.copyFile, PsiErrorElement.class).size();
    out(n + " copy: forms by status=" + st + " substituted uses=" + uses + " expanded forms broken in the copy=" + broken +
        " expanded forms whose uses are all fragments=" + allFragments + " with a complete-expression use=" + someComplete +
        " copy parse errors=" + copyErrors + " outcomes=" + c.expansion.outcomes);
    // highlighting, layer off and on
    myFixture.configureFromExistingVirtualFile(file.getVirtualFile());
    mode(ErlangMacroLayer.Mode.OFF);
    List<String> off = problems(freshHighlighting());
    mode(ErlangMacroLayer.Mode.LAYER);
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    int answered0 = ErlangMacroLayer.FALLBACK_ANSWERED.get(), hidden0 = ErlangMacroLayer.ERRORS_HIDDEN.get();
    List<String> on = problems(freshHighlighting());
    int answered = ErlangMacroLayer.FALLBACK_ANSWERED.get() - answered0, hidden = ErlangMacroLayer.ERRORS_HIDDEN.get() - hidden0;
    Set<String> gone = new TreeSet<>(off);
    on.forEach(gone::remove);
    Set<String> added = new TreeSet<>(on);
    off.forEach(added::remove);
    out(n + " errors: off=" + off.stream().filter(x -> x.startsWith("ERROR|")).count() + " on=" + on.stream().filter(x -> x.startsWith("ERROR|")).count());
    out(n + " highlighting: problems off=" + off.size() + " on=" + on.size() + " removed by the layer=" + gone.size() +
        " added=" + added.size() + " layer answers=" + answered + " errors hidden=" + hidden);
    gone.stream().limit(10).forEach(g -> out(n + "   removed " + g));
    added.stream().limit(10).forEach(g -> out(n + "   added " + g));
    // the counters of the highlighting run, by entry and outcome (all callers)
    Map<String, long[]> byOutcome = new TreeMap<>();
    ErlangMacroLayerStats.snapshot().forEach((k, v) -> {
      String[] p = k.split("\t");
      if (!p[0].equals(n) && !p[0].equals("-")) return;
      long[] a = byOutcome.computeIfAbsent(p[1] + " " + p[2], x -> new long[3]);
      a[0] += v[0];
      a[1] += v[1];
      a[2] += v[2];
    });
    byOutcome.forEach((k, v) -> out(String.format(Locale.ROOT, "%s counters %s calls=%d ms=%.1f exclusive_ms=%.1f", n, k, v[0], v[1] / 1e6, v[2] / 1e6)));
    // every reference, resolved with the layer off and on (a background read action)
    mode(ErlangMacroLayer.Mode.OFF);
    Map<String, String> r0 = resolveMap(file);
    mode(ErlangMacroLayer.Mode.LAYER);
    Map<String, String> r1 = resolveMap(file);
    int diff = 0;
    List<String> samples = new ArrayList<>();
    for (Map.Entry<String, String> e : r1.entrySet()) {
      if (!Objects.equals(e.getValue(), r0.get(e.getKey()))) {
        diff++;
        if (samples.size() < 10) samples.add(e.getKey() + " off=" + r0.get(e.getKey()) + " on=" + e.getValue());
      }
    }
    out(n + " references: " + r1.size() + ", resolved off=" + r0.values().stream().filter(Objects::nonNull).count() +
        " on=" + r1.values().stream().filter(Objects::nonNull).count() + ", different targets=" + diff);
    samples.forEach(s -> out(n + "   differs " + s));
    // which references master leaves unresolved (these are the ones the resolve hook passes to the layer)
    Map<String, Integer> unresolved = inBackground(() -> {
      Map<String, Integer> m = new TreeMap<>();
      for (PsiElement e : PsiTreeUtil.collectElements(file, x -> x.getReferences().length > 0)) {
        for (PsiReference r : e.getReferences()) {
          if (r.resolve() != null) continue;
          String kind = r.getClass().getSimpleName();
          String text = kind.contains("Variable") ? "<var>" : r.getCanonicalText();
          m.merge(kind + " " + text, 1, Integer::sum);
        }
      }
      return m;
    });
    unresolved.entrySet().stream().sorted((a, b) -> b.getValue() - a.getValue()).limit(25)
      .forEach(e -> out(n + "   unresolved (layer on) " + e.getKey() + " x" + e.getValue()));
    ErlangMacroLayerLog.enableForTests(false);
  }

  private static Map<String, String> resolveMap(PsiFile file) throws Exception {
    return inBackground(() -> {
      Map<String, String> m = new TreeMap<>();
      for (PsiElement e : PsiTreeUtil.collectElements(file, x -> x.getReferences().length > 0)) {
        PsiReference[] refs = e.getReferences();
        for (int i = 0; i < refs.length; i++) {
          PsiElement t = refs[i] instanceof PsiPolyVariantReference p && p.multiResolve(false).length > 0 ? p.multiResolve(false)[0].getElement() : refs[i].resolve();
          String where = t == null ? null : (t.getContainingFile() == null ? "?" : t.getContainingFile().getName()) + "@" + t.getTextOffset();
          m.put(e.getTextOffset() + "#" + i + " " + e.getText().substring(0, Math.min(20, e.getTextLength())), where);
        }
      }
      return m;
    });
  }
}
