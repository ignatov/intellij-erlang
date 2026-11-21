package org.intellij.erlang.macrolayer;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * The narrow gate gives the answers of the wide gate on real files. For each file: the problems (warnings and above) of
 * a highlighting from scratch and every resolve target (single and poly-variant), with the narrow and the wide gate;
 * any difference is printed. Also the forms that need the copy and the copy builds per gate.
 * Runs only when {@code S5_SOUND} and {@code MACRO_LAYER_OTP_ROOT} ({@link LayerTestSupport#otpRoot()}) are set: a file of lines "label TAB path" (the round-4 list), or "default" for
 * {@code erl_types.erl} and {@code xmerl_scan.erl}. {@code S5_SOUND_EXTRA}: more paths, comma-separated.
 * Output: lines "S5-SOUND ..." and, with {@code S5_SOUND_OUT}, a TSV file.
 */
public class ErlangMacroLayerSoundnessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OTP = LayerTestSupport.otpRoot() + "/lib";

  public ErlangMacroLayerSoundnessTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor() {
      @Override
      public com.intellij.openapi.projectRoots.Sdk getSdk() {
        return LayerTestSupport.otpOrMockSdk(System.getenv("S5_SOUND") != null);
      }
    };
  }

  private static void out(String s) {
    System.out.println("S5-SOUND " + s);
  }

  private final Set<String> myCopied = new HashSet<>();

  /** Copies src and include of an OTP app (only include when not full), an .app file and a rebar.config. */
  private void copyApp(String appDir, boolean full) throws Exception {
    String name = appDir.replaceAll("-[0-9.]+$", "");
    if (!myCopied.add(name + full)) return;
    Path app = Paths.get(OTP, appDir);
    for (String dir : full ? List.of("src", "include") : List.of("include")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).toList()) {
          String fn = p.getFileName().toString();
          String target = "apps/" + name + "/" + app.relativize(p);
          if (myFixture.findFileInTempDir(target) != null) continue;
          if (fn.endsWith(".hrl") || full && fn.endsWith(".erl")) myFixture.addFileToProject(target, read(p));
        }
      }
    }
    if (myFixture.findFileInTempDir("apps/" + name + "/ebin/" + name + ".app") == null) {
      myFixture.addFileToProject("apps/" + name + "/ebin/" + name + ".app", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
      myFixture.addFileToProject("apps/" + name + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
    }
  }

  public void testSoundness() throws Exception {
    String list = env("S5_SOUND");
    if (list == null || LayerTestSupport.otpRoot() == null) return;
    List<String> paths = new ArrayList<>();
    if (!list.equals("default")) {
      for (String l : Files.readAllLines(Paths.get(list))) {
        if (l.isBlank()) continue;
        paths.add(l.contains("\t") ? l.substring(l.indexOf('\t') + 1).trim() : l.trim());
      }
    }
    else {
      paths.add(OTP + "/dialyzer-6.0.3/src/erl_types.erl");
      paths.add(OTP + "/xmerl-2.2/src/xmerl_scan.erl");
    }
    String extra = env("S5_SOUND_EXTRA");
    if (extra != null) paths.addAll(List.of(extra.split(",")));
    setUpProjectSdk();
    enableAll(myFixture);
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    copyApp("kernel-11.0.4", false);
    copyApp("stdlib-8.1", false);
    Set<String> fullApps = new LinkedHashSet<>();
    for (String p : paths) fullApps.add(p.substring(OTP.length() + 1, p.indexOf('/', OTP.length() + 1)));
    for (String app : fullApps) copyApp(app, true);
    List<String> rows = new ArrayList<>();
    rows.add("file\tforms_with_use\tforms_needing_copy\tdiff_lines\tproblems\trefs\tlayer_answers_narrow\tbuilds_narrow\tbuilds_wide");
    int filesWithDiff = 0, totalDiff = 0, totalNeed = 0, totalUse = 0, noCopyFiles = 0;
    for (String p : paths) {
      String app = p.substring(OTP.length() + 1, p.indexOf('/', OTP.length() + 1)).replaceAll("-[0-9.]+$", "");
      String rel = "apps/" + app + "/" + p.substring(p.indexOf('/', OTP.length() + 1) + 1);
      VirtualFile vf = myFixture.findFileInTempDir(rel);
      if (vf == null) {
        out("missing " + rel);
        continue;
      }
      PsiFile file = PsiManager.getInstance(getProject()).findFile(vf);
      myFixture.configureFromExistingVirtualFile(vf);
      int[] forms = inBackground(() -> {
        ErlangMacroLayer.Table t = ErlangMacroLayer.table((ErlangFile)file);
        int use = 0, need = 0;
        if (t == null || t.refusal != null) return new int[]{-1, -1};
        for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
          if (ErlangMacroLayer.formUsesMacroForTests(t, c)) use++;
          if (ErlangMacroLayer.formNeedsCopy(t, c)) need++;
        }
        return new int[]{use, need};
      });
      ErlangMacroLayerLog.enableForTests(true);
      ErlangMacroLayerLog.reset();
      int answered0 = ErlangMacroLayer.FALLBACK_ANSWERED.get();
      String narrow = answersOf(file, false);
      int answers = ErlangMacroLayer.FALLBACK_ANSWERED.get() - answered0;
      int buildsNarrow = builds().size();
      ErlangMacroLayerLog.reset();
      String wide = answersOf(file, true);
      int buildsWide = builds().size();
      ErlangMacroLayerLog.enableForTests(false);
      List<String> a = narrow.lines().toList(), b = wide.lines().toList();
      List<String> diff = new ArrayList<>();
      Set<String> sa = new LinkedHashSet<>(a), sb = new LinkedHashSet<>(b);
      for (String l : a) if (!sb.contains(l)) diff.add("narrow only: " + l);
      for (String l : b) if (!sa.contains(l)) diff.add("wide only:   " + l);
      int problems = a.indexOf("--");
      String n = file.getName();
      out(n + " forms with a use=" + forms[0] + " needing the copy=" + forms[1] + " problems=" + problems + " lines=" + a.size() +
          " layer answers (narrow)=" + answers + " builds narrow=" + buildsNarrow + " wide=" + buildsWide + " differences=" + diff.size());
      diff.stream().limit(20).forEach(d -> out(n + "   " + d));
      rows.add(String.join("\t", rel, String.valueOf(forms[0]), String.valueOf(forms[1]), String.valueOf(diff.size()),
                           String.valueOf(problems), String.valueOf(a.size() - problems - 1), String.valueOf(answers),
                           String.valueOf(buildsNarrow), String.valueOf(buildsWide)));
      if (!diff.isEmpty()) filesWithDiff++;
      totalDiff += diff.size();
      if (forms[1] > 0) totalNeed++;
      if (forms[0] > 0) totalUse++;
      if (forms[0] > 0 && forms[1] == 0) noCopyFiles++;
      myFixture.getEditor(); // keep the editor until the next file replaces it
    }
    out("total files=" + paths.size() + " with a use=" + totalUse + " needing a copy=" + totalNeed +
        " with uses but no form needing a copy=" + noCopyFiles + " files with differences=" + filesWithDiff + " difference lines=" + totalDiff);
    String o = env("S5_SOUND_OUT");
    if (o != null) Files.write(Paths.get(o), rows, StandardCharsets.UTF_8);
    assertEquals("difference lines between the narrow and the wide gate", 0, totalDiff);
  }

  /** The answers of a file with one gate, as in {@link ErlangMacroLayerPlainGateTest}. */
  String answersOf(PsiFile f, boolean wide) throws Exception {
    com.intellij.openapi.Disposable d = com.intellij.openapi.util.Disposer.newDisposable();
    try {
      ErlangMacroLayer.setGateWideForTests(wide, d);
      PsiManager.getInstance(getProject()).dropPsiCaches();
      com.intellij.codeInsight.daemon.DaemonCodeAnalyzer.getInstance(getProject()).restart();
      List<String> problems = new ArrayList<>();
      for (com.intellij.codeInsight.daemon.impl.HighlightInfo i : myFixture.doHighlighting()) {
        if (i.getSeverity().compareTo(com.intellij.lang.annotation.HighlightSeverity.WEAK_WARNING) < 0 || i.getDescription() == null) continue;
        problems.add(i.getStartOffset() + ":" + i.getDescription());
      }
      Collections.sort(problems);
      return String.join("\n", problems) + (problems.isEmpty() ? "" : "\n") + "--\n" + ErlangMacroLayerPlainGateTest.resolveTargets(f);
    }
    finally {
      com.intellij.openapi.util.Disposer.dispose(d);
    }
  }
}
