package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.rename.RenameProcessor;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.ref.WeakReference;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Spike 4: timings T1 to T4, memory M1, headers (C6, problems, go-to, rename, seeds) on real app fixtures.
 * Small-IDE mode, so that includes resolve through the app root and rebar.config as in a real project.
 * Every method runs only when its environment variable is set (S4_TIMING, S4_T3, S4_T4, S4_M1, S4_HEADERS, S4_C6).
 */
public class ErlangMacroLayerPerfTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib";
  static final String CORPUS = "/Users/ignatov/factory-work/macro-research/count/corpus";
  private int myN;
  private Disposable myMode;
  private final Set<String> myCopiedApps = new HashSet<>();

  public ErlangMacroLayerPerfTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    mode(ErlangMacroLayer.Mode.LAYER);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      out(getName() + " ui-thread builds=" + uiThreadBuilds().size());
      if (myMode != null) Disposer.dispose(myMode);
      ErlangMacroLayerLog.enableForTests(false);
    }
    finally {
      super.tearDown();
    }
  }

  static void out(String s) {
    System.out.println("LAYER-PERF " + s);
  }

  private void mode(ErlangMacroLayer.Mode m) {
    if (myMode != null) Disposer.dispose(myMode);
    myMode = Disposer.newDisposable();
    ErlangMacroLayer.setModeForTests(m, myMode);
  }

  // ------------------------------------------------------------------------------------------------ fixtures

  static Path findApp(String name) throws Exception {
    try (Stream<Path> s = Files.list(Paths.get(OTP))) {
      Optional<Path> otp = s.filter(p -> p.getFileName().toString().matches(Pattern.quote(name) + "-[0-9.]+")).findFirst();
      if (otp.isPresent()) return otp.get();
    }
    for (String sub : List.of("apps", "vendor", "deps", "_checkouts", "")) {
      try (Stream<Path> s = Files.list(Paths.get(CORPUS))) {
        for (Path proj : s.toList()) {
          Path p = sub.isEmpty() ? proj : proj.resolve(sub).resolve(name);
          if (sub.isEmpty() && !proj.getFileName().toString().equals(name)) continue;
          if (Files.isDirectory(p.resolve("src")) || Files.isDirectory(p.resolve("include"))) return p;
        }
      }
    }
    return null;
  }

  private static final Pattern INCLUDE_LIB = Pattern.compile("-include_lib\\(\\s*\"([^/\"]+)/");

  /** Copies an app: src and include (.erl, .hrl), an .app file, and a rebar.config with the include paths. */
  private String copyApp(Path app, String name, boolean full) throws Exception {
    String root = "apps/" + name;
    if (!myCopiedApps.add(name + full)) return root;
    Set<String> libs = new TreeSet<>();
    for (String dir : full ? List.of("src", "include") : List.of("include")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).toList()) {
          String fn = p.getFileName().toString();
          if (!fn.endsWith(".erl") && !fn.endsWith(".hrl")) continue;
          String text = read(p);
          myFixture.addFileToProject(root + "/" + app.relativize(p), text);
          Matcher m = INCLUDE_LIB.matcher(text);
          while (m.find()) libs.add(m.group(1));
        }
      }
    }
    myFixture.addFileToProject(root + "/ebin/" + name + ".app", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
    myFixture.addFileToProject(root + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
    for (String lib : libs) {
      if (lib.equals(name) || myCopiedApps.contains(lib + "false") || myCopiedApps.contains(lib + "true")) continue;
      Path l = findApp(lib);
      if (l != null) copyApp(l, lib, false);
    }
    return root;
  }

  private PsiFile psi(String path) {
    VirtualFile v = myFixture.findFileInTempDir(path);
    return v == null ? null : PsiManager.getInstance(getProject()).findFile(v);
  }

  private List<HighlightInfo> highlight(VirtualFile v) {
    myFixture.configureFromExistingVirtualFile(v);
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

  private long buildNanosSince(int fromEvent) {
    List<ErlangMacroLayerLog.Event> ev = ErlangMacroLayerLog.events();
    long n = 0;
    for (ErlangMacroLayerLog.Event e : ev.subList(fromEvent, ev.size())) if (e.kind().equals("BUILD") && !e.trigger().equals("direct")) n += e.nanos();
    return n;
  }

  private int buildsSince(int fromEvent) {
    List<ErlangMacroLayerLog.Event> ev = ErlangMacroLayerLog.events();
    int n = 0;
    for (ErlangMacroLayerLog.Event e : ev.subList(fromEvent, ev.size())) if (e.kind().equals("BUILD") && !e.trigger().equals("direct")) n++;
    return n;
  }

  // ------------------------------------------------------------------------------------------------ T1, T2

  /** The start of the body of the first clause of the largest function: inside a large function. */
  private static int largestFunctionBody(PsiFile f) {
    ErlangFunction best = null;
    for (ErlangFunction fn : ((ErlangFile)f).getFunctions()) if (best == null || fn.getTextLength() > best.getTextLength()) best = fn;
    if (best == null) return f.getTextLength();
    ErlangFunctionClause c = best.getFunctionClauseList().getFirst();
    ErlangClauseBody body = c.getClauseBody();
    return body != null ? body.getTextRange().getStartOffset() : c.getTextRange().getEndOffset();
  }

  public void testTimingT1T2() throws Exception {
    if (env("S4_TIMING") == null) return;
    enableAll(myFixture);
    String xmerl = copyApp(findApp("xmerl"), "xmerl", true);
    String wx = copyApp(findApp("wx"), "wx", true);
    String[][] files = {
      {OTP + "/dialyzer-6.0.3/src/erl_types.erl", "dialyzer/src", "t_inf_lists_strict(L1, L2) ->\n"},
      {OTP + "/xmerl-2.2/src/xmerl_scan.erl", xmerl + "/src", "    ets:delete(Rules),\n"},
      {OTP + "/wx-2.7/src/gen/wxStyledTextCtrl.erl", wx + "/src/gen", null}};
    int reps = Integer.parseInt(Objects.requireNonNullElse(env("S4_REPS"), "7"));
    for (String[] f : files) {
      String text = read(Paths.get(f[0]));
      String name = Paths.get(f[0]).getFileName().toString();
      Map<String, List<Double>> m = new TreeMap<>();
      for (int rep = 0; rep < reps; rep++) {
        List<ErlangMacroLayer.Mode> order = rep % 2 == 0 ? List.of(ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER)
                                                         : List.of(ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.Mode.OFF);
        for (ErlangMacroLayer.Mode mode : order) {
          mode(mode);
          String k = mode.name().toLowerCase(Locale.ROOT);
          PsiFile file = myFixture.addFileToProject(f[1] + "/t" + (myN++) + "_" + name, text);
          VirtualFile v = file.getVirtualFile();
          highlight(v);
          int anchor = f[2] != null ? text.indexOf(f[2]) + f[2].length() : largestFunctionBody(file);
          // (a) keystroke with commit
          myFixture.getEditor().getCaretModel().moveToOffset(anchor);
          String typed = "        foo(Bar),\n";
          long t0 = System.nanoTime();
          for (char c : typed.toCharArray()) {
            myFixture.type(c);
            PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
          }
          m.computeIfAbsent("T1a commit " + k, x -> new ArrayList<>()).add((System.nanoTime() - t0) / 1e6 / typed.length());
          // (b) keystroke followed by full highlighting
          String typed2 = "x1(Y),\n";
          int e0 = ErlangMacroLayerLog.events().size();
          long q0 = ErlangMacroLayerLog.queryNanosTotal();
          t0 = System.nanoTime();
          for (char c : typed2.toCharArray()) {
            myFixture.type(c);
            PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
            myFixture.doHighlighting();
          }
          m.computeIfAbsent("T1b key+highlight " + k, x -> new ArrayList<>()).add((System.nanoTime() - t0) / 1e6 / typed2.length());
          if (mode == ErlangMacroLayer.Mode.LAYER) {
            m.computeIfAbsent("T1b layer build ms/key", x -> new ArrayList<>()).add(buildNanosSince(e0) / 1e6 / typed2.length());
            m.computeIfAbsent("T1b layer builds/key", x -> new ArrayList<>()).add((double)buildsSince(e0) / typed2.length());
            m.computeIfAbsent("T1b layer query ms/key", x -> new ArrayList<>()).add((ErlangMacroLayerLog.queryNanosTotal() - q0) / 1e6 / typed2.length());
          }
          // (c) T2: an unfinished call inside the largest function
          int big = largestFunctionBody(myFixture.getFile());
          myFixture.getEditor().getCaretModel().moveToOffset(big);
          String typed3 = "foo(Bar, baz(";
          e0 = ErlangMacroLayerLog.events().size();
          q0 = ErlangMacroLayerLog.queryNanosTotal();
          t0 = System.nanoTime();
          for (char c : typed3.toCharArray()) {
            myFixture.type(c);
            PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
            myFixture.doHighlighting();
          }
          m.computeIfAbsent("T2 unfinished+highlight " + k, x -> new ArrayList<>()).add((System.nanoTime() - t0) / 1e6 / typed3.length());
          if (mode == ErlangMacroLayer.Mode.LAYER) {
            m.computeIfAbsent("T2 layer build ms/key", x -> new ArrayList<>()).add(buildNanosSince(e0) / 1e6 / typed3.length());
            m.computeIfAbsent("T2 layer builds/key", x -> new ArrayList<>()).add((double)buildsSince(e0) / typed3.length());
            m.computeIfAbsent("T2 layer query ms/key", x -> new ArrayList<>()).add((ErlangMacroLayerLog.queryNanosTotal() - q0) / 1e6 / typed3.length());
          }
          FileEditorManager.getInstance(getProject()).closeFile(v);
          WriteAction.run(() -> v.delete(this));
        }
        out("T1T2 " + name + " rep " + rep + " done load=" + load());
      }
      for (Map.Entry<String, List<Double>> e : m.entrySet()) out("T1T2 " + name + " " + e.getKey() + " " + stats(e.getValue()));
      out("T1T2 " + name + " ratio T1b=" + ratio(m, "T1b key+highlight ") + " T2=" + ratio(m, "T2 unfinished+highlight ") +
          " T1a=" + ratio(m, "T1a commit ") + " load=" + load());
    }
  }

  private static String ratio(Map<String, List<Double>> m, String prefix) {
    double off = median(m.get(prefix + "off")), layer = median(m.get(prefix + "layer"));
    return String.format(Locale.ROOT, "%+.1f%%", (layer / off - 1) * 100);
  }

  // ------------------------------------------------------------------------------------------------ T3

  public void testFirstHighlightingT3() throws Exception {
    if (env("S4_T3") == null) return;
    enableAll(myFixture);
    String[] files = {OTP + "/stdlib-8.1/src/erl_parse.erl", OTP + "/stdlib-8.1/src/unicode_util.erl",
      OTP + "/megaco-4.9.2/src/text/megaco_text_parser_v3.erl", OTP + "/megaco-4.9.2/src/text/megaco_text_parser_v2.erl",
      OTP + "/megaco-4.9.2/src/text/megaco_text_parser_v1.erl", CORPUS + "/ejabberd/src/ejabberd_commands_doc.erl"};
    int reps = Integer.parseInt(Objects.requireNonNullElse(env("S4_REPS"), "7"));
    for (String path : files) {
      Path p = Paths.get(path);
      String name = p.getFileName().toString();
      String dir = "t3/" + name.replace(".erl", "");
      // the headers of the file's own directory and of ../include, next to it (relative includes resolve)
      for (Path hd : List.of(p.getParent(), p.getParent().getParent().resolve("include"))) {
        if (!Files.isDirectory(hd)) continue;
        try (Stream<Path> s = Files.list(hd)) {
          for (Path h : s.filter(x -> x.toString().endsWith(".hrl")).toList()) myFixture.addFileToProject(dir + "/" + h.getFileName(), read(h));
        }
      }
      String text = read(p);
      Map<String, List<Double>> m = new TreeMap<>();
      for (int rep = 0; rep < reps; rep++) {
        for (ErlangMacroLayer.Mode mode : rep % 2 == 0 ? List.of(ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER)
                                                       : List.of(ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.Mode.OFF)) {
          mode(mode);
          PsiFile f = myFixture.addFileToProject(dir + "/r" + (myN++) + "_" + name, text);
          int e0 = ErlangMacroLayerLog.events().size();
          long t0 = System.nanoTime();
          highlight(f.getVirtualFile());
          m.computeIfAbsent(mode.name().toLowerCase(Locale.ROOT), x -> new ArrayList<>()).add((System.nanoTime() - t0) / 1e6);
          if (mode == ErlangMacroLayer.Mode.LAYER) {
            m.computeIfAbsent("layer build ms", x -> new ArrayList<>()).add(buildNanosSince(e0) / 1e6);
            m.computeIfAbsent("layer builds", x -> new ArrayList<>()).add((double)buildsSince(e0));
          }
          VirtualFile v = f.getVirtualFile();
          FileEditorManager.getInstance(getProject()).closeFile(v);
          WriteAction.run(() -> v.delete(this));
        }
      }
      PsiFile probe = myFixture.addFileToProject(dir + "/cap_" + name, text);
      ErlangMacroLayer.Copy c = inBackground(() -> ErlangMacroLayer.build(probe));
      int over = 0, expanded = 0;
      for (ErlangMacroExpander.Form fm : c.expansion.forms) {
        if (fm.status == ErlangMacroExpander.FormStatus.OVER_CAP) over++;
        if (fm.status == ErlangMacroExpander.FormStatus.EXPANDED) {
          expanded++;
          assertTrue("form cap", fm.expandedTokens <= ErlangMacroLayer.FORM_CAP);
        }
      }
      for (Map.Entry<String, List<Double>> e : m.entrySet()) out("T3 " + name + " " + e.getKey() + " " + stats(e.getValue()));
      out("T3 " + name + " ratio=" + String.format(Locale.ROOT, "%+.1f%%", (median(m.get("layer")) / median(m.get("off")) - 1) * 100) +
          " expandedForms=" + expanded + " overCap=" + over + " refusal=" + ErlangMacroLayer.refusalForTests(probe) + " load=" + load());
    }
  }

  // ------------------------------------------------------------------------------------------------ T4

  public void testStressT4() throws Exception {
    if (env("S4_T4") == null) return;
    enableAll(myFixture);
    String wx = copyApp(findApp("wx"), "wx", true);
    PsiFile header = psi(wx + "/src/wxe.hrl");
    List<VirtualFile> includers = new ArrayList<>();
    VirtualFile gen = myFixture.findFileInTempDir(wx + "/src/gen");
    List<VirtualFile> all = new ArrayList<>(List.of(gen.getChildren()));
    all.sort(Comparator.comparing(VirtualFile::getName));
    for (VirtualFile v : all) {
      if (!v.getName().endsWith(".erl")) continue;
      String t = PsiManager.getInstance(getProject()).findFile(v).getText();
      if (t.contains("-include(\"wxe.hrl\")") && t.contains("?get_env")) includers.add(v);
      if (includers.size() == 20) break;
    }
    int countAll = 0;
    for (VirtualFile v : all) if (v.getName().endsWith(".erl") && PsiManager.getInstance(getProject()).findFile(v).getText().contains("wxe.hrl")) countAll++;
    out("T4 wx includers of wxe.hrl in the project=" + countAll + ", open=" + includers.size());
    Map<String, List<Double>> m = new TreeMap<>();
    int reps = Integer.parseInt(Objects.requireNonNullElse(env("S4_REPS"), "7"));
    Document d = PsiDocumentManager.getInstance(getProject()).getDocument(header);
    String line = "-define(BATCH_BEGIN,    5).";
    for (int rep = 0; rep < reps + 1; rep++) {
      for (ErlangMacroLayer.Mode mode : rep % 2 == 0 ? List.of(ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER)
                                                     : List.of(ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.Mode.OFF)) {
        mode(mode);
        for (VirtualFile v : includers) highlight(v);
        int at = d.getText().indexOf("-define(BATCH_BEGIN,");
        int end = d.getText().indexOf('\n', at);
        String now = d.getText().substring(at, end);
        String next = now.equals(line) ? "-define(BATCH_BEGIN,    7)." : line;
        WriteCommandAction.runWriteCommandAction(getProject(), () -> d.replaceString(at, end, next));
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        int e0 = ErlangMacroLayerLog.events().size();
        long t0 = System.nanoTime();
        for (VirtualFile v : includers) highlight(v);
        double ms = (System.nanoTime() - t0) / 1e6;
        if (rep == 0) continue; // warm-up
        String k = mode.name().toLowerCase(Locale.ROOT);
        m.computeIfAbsent(k, x -> new ArrayList<>()).add(ms);
        if (mode == ErlangMacroLayer.Mode.LAYER) {
          m.computeIfAbsent("layer builds", x -> new ArrayList<>()).add((double)buildsSince(e0));
          m.computeIfAbsent("layer build ms", x -> new ArrayList<>()).add(buildNanosSince(e0) / 1e6);
        }
      }
      out("T4 rep " + rep + " load=" + load());
    }
    for (Map.Entry<String, List<Double>> e : m.entrySet()) out("T4 " + e.getKey() + " " + stats(e.getValue()));
    out("T4 ratio=" + String.format(Locale.ROOT, "%+.1f%%", (median(m.get("layer")) / median(m.get("off")) - 1) * 100) + " load=" + load());
  }

  // ------------------------------------------------------------------------------------------------ M1

  private static long usedAfterGc() throws Exception {
    long best = Long.MAX_VALUE;
    for (int i = 0; i < 4; i++) {
      System.gc();
      Thread.sleep(150);
      Runtime r = Runtime.getRuntime();
      best = Math.min(best, r.totalMemory() - r.freeMemory());
    }
    return best;
  }

  public void testMemoryM1() throws Exception {
    String list = env("S4_M1");
    if (list == null) return;
    enableAll(myFixture);
    List<Path> paths = new ArrayList<>();
    for (String l : Files.readAllLines(Paths.get(list))) if (!l.isBlank()) paths.add(Paths.get(l.trim()));
    List<WeakReference<ErlangMacroLayer.Copy>> copies = new ArrayList<>();
    List<VirtualFile> opened = new ArrayList<>();
    long chars = 0;
    long t0 = System.nanoTime();
    for (Path p : paths) {
      PsiFile f = myFixture.addFileToProject("m1/" + (myN++) + "/" + p.getFileName(), read(p));
      highlight(f.getVirtualFile());
      opened.add(f.getVirtualFile());
      ErlangMacroLayer.Copy c = ErlangMacroLayer.peekCopy(f);
      if (c != null) {
        copies.add(new WeakReference<>(c));
        chars += c.expansion.text.length();
      }
    }
    out("M1 files=" + paths.size() + " copies built=" + copies.size() + " expanded chars=" + chars + " s=" + (System.nanoTime() - t0) / 1_000_000_000);
    long h1 = usedAfterGc();
    int alive1 = (int)copies.stream().filter(w -> w.get() != null).count();
    int strong = ErlangMacroLayerRetention.getInstance(getProject()).strongCount();
    ErlangMacroLayerRetention.getInstance(getProject()).releaseAll();
    long h2 = usedAfterGc();
    int alive2 = (int)copies.stream().filter(w -> w.get() != null).count();
    out("M1 after GC: alive copies=" + alive1 + " strongly held=" + strong + " used heap MB=" + h1 / 1048576 +
        "; after releasing the strong ones and GC: alive copies=" + alive2 + " used heap MB=" + h2 / 1048576 +
        " => retained by " + (alive1 - alive2) + " copies MB=" + (h1 - h2) / 1048576 +
        " per copy KB=" + (alive1 - alive2 == 0 ? 0 : (h1 - h2) / 1024 / (alive1 - alive2)));
    for (VirtualFile v : opened) FileEditorManager.getInstance(getProject()).closeFile(v);
    long h3 = usedAfterGc();
    int alive3 = (int)copies.stream().filter(w -> w.get() != null).count();
    out("M1 after closing all editors and GC: alive copies=" + alive3 + " used heap MB=" + h3 / 1048576 + " load=" + load());
  }

  // ------------------------------------------------------------------------------------------------ headers

  record HeaderCase(String app, String header) {
  }

  static final List<HeaderCase> CASES = List.of(new HeaderCase("ssh", "ssh.hrl"), new HeaderCase("rebar", "rebar.hrl"),
                                                new HeaderCase("relx", "relx.hrl"), new HeaderCase("emqx_resource", "emqx_resource_id.hrl"),
                                                new HeaderCase("emqx_durable_storage", "emqx_ds_pmap.hrl"), new HeaderCase("wx", "wxe.hrl"));

  private List<VirtualFile> includers(String root, String header, int max) throws Exception {
    List<VirtualFile> r = new ArrayList<>();
    VirtualFile dir = myFixture.findFileInTempDir(root);
    com.intellij.openapi.vfs.VfsUtilCore.iterateChildrenRecursively(dir, x -> true, v -> {
      if (v.getName().endsWith(".erl") && r.size() < max) {
        String t = PsiManager.getInstance(getProject()).findFile(v).getText();
        if (Pattern.compile("-include(_lib)?\\(\\s*\"[^\"]*" + Pattern.quote(header) + "\"").matcher(t).find()) r.add(v);
      }
      return true;
    });
    r.sort(Comparator.comparing(VirtualFile::getPath));
    return r;
  }

  private int loadedHeaderTrees() {
    int[] n = {0};
    com.intellij.openapi.vfs.VfsUtilCore.iterateChildrenRecursively(myFixture.findFileInTempDir("apps"), x -> true, v -> {
      if (v.getName().endsWith(".hrl")) {
        PsiFile f = PsiManager.getInstance(getProject()).findFile(v);
        if (f instanceof PsiFileImpl impl && impl.getTreeElement() != null) n[0]++;
      }
      return true;
    });
    return n[0];
  }

  private void c6(ErlangMacroLayer.Mode mode) throws Exception {
    if (env("S4_C6") == null) return;
    enableAll(myFixture);
    mode(mode);
    List<VirtualFile> files = new ArrayList<>();
    for (HeaderCase hc : CASES) {
      String root = copyApp(findApp(hc.app), hc.app, true);
      files.addAll(includers(root, hc.header, 15));
    }
    int before = loadedHeaderTrees();
    for (VirtualFile v : files) highlight(v);
    out("C6 mode=" + mode + " includers highlighted=" + files.size() + " header PSI trees loaded: before=" + before + " after=" +
        loadedHeaderTrees() + " header targets materialized=" + ErlangMacroLayer.HEADER_TARGETS_MATERIALIZED.get() +
        " summaries built=" + ErlangMacroLayer.SUMMARIES_BUILT.get() + " builds=" + builds().size());
  }

  public void testC6HeaderTreesMaster() throws Exception {
    c6(ErlangMacroLayer.Mode.OFF);
  }

  public void testC6HeaderTreesLayer() throws Exception {
    c6(ErlangMacroLayer.Mode.LAYER);
  }

  public void testHeaders() throws Exception {
    if (env("S4_HEADERS") == null) return;
    enableAll(myFixture);
    Path outDir = Paths.get(env("S4_HEADERS"));
    Files.createDirectories(outDir);
    StringBuilder diff = new StringBuilder();
    for (HeaderCase hc : CASES) {
      String root = copyApp(findApp(hc.app), hc.app, true);
      List<VirtualFile> files = includers(root, hc.header, 20);
      int fileIndex = 0;
      int added = 0, removed = 0, parserHidden = 0, refused = 0, gainedVars = 0, headerTargets = 0, gotoOk = 0;
      int macroGotoOff = 0, macroGotoLayer = 0, uses = 0, ambiguous = 0, conditional = 0, seeds = 0, seedsShownOff = 0, seedsHidden = 0;
      Map<String, Integer> refusals = new TreeMap<>();
      for (VirtualFile v : files) {
        PsiFile f = PsiManager.getInstance(getProject()).findFile(v);
        String refusal = ErlangMacroLayer.refusalForTests(f);
        if (refusal != null) {
          refused++;
          refusals.merge(refusal, 1, Integer::sum);
        }
        ErlangMacroLayer.Copy c = inBackground(() -> ErlangMacroLayer.build(f));
        uses += c.expansion.headerUses;
        ambiguous += c.expansion.headerAmbiguous;
        conditional += c.expansion.headerConditional;
        mode(ErlangMacroLayer.Mode.OFF);
        List<String> off = problems(highlight(v));
        Map<PsiElement, PsiElement> offVars = varTargets(f);
        int offMacro = macroGoto(f);
        mode(ErlangMacroLayer.Mode.LAYER);
        List<String> layer = problems(highlight(v));
        Map<PsiElement, PsiElement> layerVars = varTargets(f);
        int layerMacro = macroGoto(f);
        macroGotoOff += offMacro;
        macroGotoLayer += layerMacro;
        List<String> a = new ArrayList<>(layer);
        a.removeAll(off);
        List<String> r = new ArrayList<>(off);
        r.removeAll(layer);
        added += a.size();
        removed += r.size();
        for (String x : r) {
          if (x.startsWith("ERROR") && isParserMessage(x)) parserHidden++;
          diff.append(hc.header).append(" REMOVED ").append(v.getName()).append(' ').append(context(f, x)).append('\n');
        }
        for (String x : a) diff.append(hc.header).append(" ADDED ").append(v.getName()).append(' ').append(context(f, x)).append('\n');
        for (Map.Entry<PsiElement, PsiElement> e : layerVars.entrySet()) {
          if (offVars.get(e.getKey()) != null) continue;
          gainedVars++;
          if (e.getValue() instanceof ErlangMacroLayerHeaderTarget ht) {
            headerTargets++;
            PsiElement nav = inBackground(ht::getNavigationElement);
            if (nav != null && nav.getText().equals(e.getKey().getText())) gotoOk++;
          }
        }
        // seeds in forms that use a header macro
        int[] s = fileIndex++ < 3 ? seed(hc, v, f, c) : new int[3];
        seeds += s[0];
        seedsShownOff += s[1];
        seedsHidden += s[2];
      }
      String rename = renameInHeader(root, hc.header);
      out("H " + hc.header + " includers=" + files.size() + " refused=" + refused + " " + refusals +
          " headerUses=" + uses + " lostAmbiguous=" + ambiguous + " lostConditional=" + conditional +
          " problems added=" + added + " removed=" + removed + " (parser errors hidden=" + parserHidden + ")" +
          " varsGained=" + gainedVars + " headerTargets=" + headerTargets + " gotoOk=" + gotoOk +
          " macroGoto off=" + macroGotoOff + " layer=" + macroGotoLayer +
          " seeds=" + seeds + " shownOff=" + seedsShownOff + " hiddenByLayer=" + seedsHidden + " renameInHeader=" + rename +
          " load=" + load());
    }
    Files.writeString(outDir.resolve("problems-diff.txt"), diff.toString());
  }

  private static boolean isParserMessage(String p) {
    String d = p.substring(p.indexOf('|', p.indexOf('|') + 1) + 1);
    return d.contains("expected") || d.contains("unexpected") || d.startsWith("<");
  }

  private static String context(PsiFile f, String problem) {
    int off = Integer.parseInt(problem.split("\\|")[1]);
    String t = f.getText();
    int ls = t.lastIndexOf('\n', Math.max(0, off - 1)) + 1, le = t.indexOf('\n', off);
    int line = (int)t.substring(0, off).chars().filter(ch -> ch == '\n').count() + 1;
    return problem + " @" + line + ": " + t.substring(ls, le < 0 ? t.length() : le).strip();
  }

  private Map<PsiElement, PsiElement> varTargets(PsiFile f) throws Exception {
    return inBackground(() -> {
      Map<PsiElement, PsiElement> m = new LinkedHashMap<>();
      for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(f, ErlangQVar.class)) {
        PsiReference r = v.getReference();
        if (r != null) m.put(v, r.resolve());
      }
      return m;
    });
  }

  private int macroGoto(PsiFile f) throws Exception {
    return inBackground(() -> {
      int n = 0;
      for (ErlangMacrosName mn : PsiTreeUtil.findChildrenOfType(f, ErlangMacrosName.class)) {
        PsiReference r = mn.getReference();
        PsiElement t = r == null ? null : r.resolve();
        if (t != null && t.getContainingFile() != null && t.getContainingFile().getName().endsWith(".hrl")) n++;
      }
      return n;
    });
  }

  /** Seeds 3 true errors in up to 4 forms that use a macro of the header; returns {seeds, shown by master, hidden by layer}. */
  private int[] seed(HeaderCase hc, VirtualFile v, PsiFile f, ErlangMacroLayer.Copy c) throws Exception {
    Set<String> headerNames = new HashSet<>();
    PsiFile h = null;
    for (VirtualFile x : includersHeader(hc.header)) h = PsiManager.getInstance(getProject()).findFile(x);
    if (h == null) return new int[3];
    for (ErlangMacroExpander.Directive d : ErlangMacroLayer.summaryOf(h).directives) if (d.attribute.equals("define")) headerNames.add(d.name());
    int seeds = 0, shown = 0, hidden = 0, forms = 0;
    String text = f.getText();
    for (ErlangFunction fn : ((ErlangFile)f).getFunctions()) {
      if (forms >= 4) break;
      if (!ErlangMacroLayer.usesKnownMacro(fn.getText(), headerNames)) continue;
      ErlangClauseBody body = fn.getFunctionClauseList().getFirst().getClauseBody();
      if (body == null) continue;
      forms++;
      int at = body.getTextRange().getStartOffset();
      for (String s : List.of("foo bar ", "zz_undefined_fn_q(), ", "ZzUnboundQ, ")) {
        String seeded = text.substring(0, at) + s + text.substring(at);
        PsiFile sf = myFixture.addFileToProject(v.getParent().getPath().substring(myFixture.getTempDirFixture().getTempDirPath().length() + 1) +
                                                "/seed" + (myN++) + "_" + v.getName(), seeded);
        mode(ErlangMacroLayer.Mode.OFF);
        boolean off = shownAt(problems(highlight(sf.getVirtualFile())), at, s.length());
        mode(ErlangMacroLayer.Mode.LAYER);
        boolean layer = shownAt(problems(highlight(sf.getVirtualFile())), at, s.length());
        seeds++;
        if (off) shown++;
        if (off && !layer) hidden++;
        VirtualFile sv = sf.getVirtualFile();
        FileEditorManager.getInstance(getProject()).closeFile(sv);
        WriteAction.run(() -> sv.delete(this));
      }
    }
    return new int[]{seeds, shown, hidden};
  }

  private List<VirtualFile> includersHeader(String header) {
    List<VirtualFile> r = new ArrayList<>();
    com.intellij.openapi.vfs.VfsUtilCore.iterateChildrenRecursively(myFixture.findFileInTempDir("apps"), x -> true, v -> {
      if (v.getName().equals(header)) r.add(v);
      return true;
    });
    return r;
  }

  private static boolean shownAt(List<String> problems, int at, int len) {
    for (String p : problems) {
      int o = Integer.parseInt(p.split("\\|")[1]);
      if (o >= at - 1 && o <= at + len + 1) return true;
    }
    return false;
  }

  /** Renames the first header macro used by the includers, in both modes from the same text; same result = "same". */
  private String renameInHeader(String root, String header) throws Exception {
    List<VirtualFile> hs = includersHeader(header);
    if (hs.isEmpty()) return "no-header";
    PsiFile h = PsiManager.getInstance(getProject()).findFile(hs.getFirst());
    List<ErlangMacrosDefinition> defs = new ArrayList<>(PsiTreeUtil.findChildrenOfType(h, ErlangMacrosDefinition.class));
    if (defs.isEmpty()) return "no-define";
    String name = defs.getFirst().getName();
    List<VirtualFile> files = includers(root, header, 40);
    files.add(hs.getFirst());
    Map<VirtualFile, String> original = new LinkedHashMap<>();
    for (VirtualFile v : files) original.put(v, PsiManager.getInstance(getProject()).findFile(v).getText());
    Map<ErlangMacroLayer.Mode, Map<VirtualFile, String>> results = new EnumMap<>(ErlangMacroLayer.Mode.class);
    for (ErlangMacroLayer.Mode mode : List.of(ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER)) {
      mode(mode);
      PsiFile hh = PsiManager.getInstance(getProject()).findFile(hs.getFirst());
      ErlangMacrosDefinition def = PsiTreeUtil.findChildrenOfType(hh, ErlangMacrosDefinition.class).iterator().next();
      new RenameProcessor(getProject(), def, name + "_RENAMED", false, false).run();
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      Map<VirtualFile, String> texts = new LinkedHashMap<>();
      for (VirtualFile v : files) texts.put(v, PsiManager.getInstance(getProject()).findFile(v).getText());
      results.put(mode, texts);
      WriteCommandAction.runWriteCommandAction(getProject(), () -> {
        for (Map.Entry<VirtualFile, String> e : original.entrySet()) {
          Document d = PsiDocumentManager.getInstance(getProject()).getDocument(PsiManager.getInstance(getProject()).findFile(e.getKey()));
          d.setText(e.getValue());
        }
      });
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
    int changed = 0;
    for (VirtualFile v : files) if (!results.get(ErlangMacroLayer.Mode.OFF).get(v).equals(original.get(v))) changed++;
    return (results.get(ErlangMacroLayer.Mode.OFF).equals(results.get(ErlangMacroLayer.Mode.LAYER)) ? "same" : "DIFFERENT") +
           "(" + name + ", files changed=" + changed + ")";
  }
}
