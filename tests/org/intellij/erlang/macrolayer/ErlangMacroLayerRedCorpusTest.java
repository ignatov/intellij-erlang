package org.intellij.erlang.macrolayer;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: how many "the expansion of ?M is not valid Erlang" errors the layer shows on a corpus of files that erlc
 * compiles (each one is a false red). Runs only when {@code RED_CORPUS} is set: "otp" (every application of the OTP
 * 29.1 installation) or a directory (a project: every .erl, .hrl, .app.src under it; {@code RED_DEPS}: a directory of
 * dependency checkouts copied too, not counted). Output: lines "RED ..." and, with {@code RED_OUT}, a TSV file of the
 * errors and of the files the layer refuses (with the reason).
 */
public class ErlangMacroLayerRedCorpusTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangMacroLayerRedCorpusTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor() {
      @Override
      public com.intellij.openapi.projectRoots.Sdk getSdk() {
        return org.intellij.erlang.sdk.ErlangSdkType.createMockSdk(ErlangMacroLayerSoundnessTest.OTP_ROOT,
                                                                   org.intellij.erlang.sdk.ErlangSdkRelease.V_29_0);
      }
    };
  }

  private static void out(String s) {
    System.out.println("RED " + s);
  }

  private final List<String> myErl = new ArrayList<>();

  private void copyTree(Path root, String prefix, boolean otpApp) throws Exception {
    try (Stream<Path> s = Files.walk(root)) {
      for (Path p : s.filter(Files::isRegularFile).toList()) {
        String rel = root.relativize(p).toString();
        if (rel.contains("/_build/") || rel.startsWith("_build/") || rel.contains("/.git/")) continue;
        String fn = p.getFileName().toString();
        boolean erl = fn.endsWith(".erl"), hrl = fn.endsWith(".hrl"), app = fn.endsWith(".app.src") || fn.endsWith(".app");
        if (otpApp && !(rel.startsWith("src/") || rel.startsWith("include/") || rel.startsWith("ebin/"))) continue;
        if (!erl && !hrl && !app) continue;
        String target = prefix + rel;
        myFixture.addFileToProject(target, read(p));
        if (erl) myErl.add(target);
        // an application root (the directory of src/): include paths as rebar3 gives them (the small IDE reads them)
        int src = target.lastIndexOf("/src/");
        if (src >= 0) {
          String appRoot = target.substring(0, src);
          if (myRebarConfigs.add(appRoot)) {
            myFixture.addFileToProject(appRoot + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}, {i, \"..\"}]}.\n");
          }
        }
      }
    }
  }

  private final Set<String> myRebarConfigs = new HashSet<>();

  /** The include strings of a file that master's resolution does not resolve (for the refusal report). */
  private static String unresolvedIncludes(ErlangFile file) {
    List<String> r = new ArrayList<>();
    for (org.intellij.erlang.psi.ErlangInclude inc : file.getIncludes()) {
      if (org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file).isEmpty()) r.add(inc.getText());
    }
    for (org.intellij.erlang.psi.ErlangIncludeLib inc : file.getIncludeLibs()) {
      if (org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file).isEmpty()) r.add(inc.getText());
    }
    return r.isEmpty() ? "(in a header)" : String.join(" ", r).replace('\n', ' ').replace('\t', ' ');
  }

  public void testRedCorpus() throws Exception {
    String corpus = env("RED_CORPUS");
    if (corpus == null) return;
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    if ("off".equals(env("RED_BODIES"))) ErlangMacroLayerExpansionErrors.setCheckBodiesForTests(false, getTestRootDisposable());
    if ("off".equals(env("RED_PLAIN"))) ErlangMacroLayerExpansionErrors.setChecksPlainFormsForTests(false, getTestRootDisposable());
    out("plain forms checked: " + ErlangMacroLayerExpansionErrors.checksPlainForms());
    long t0 = System.nanoTime();
    if (corpus.equals("otp")) {
      try (Stream<Path> s = Files.list(Paths.get(ErlangMacroLayerSoundnessTest.OTP))) {
        for (Path app : s.filter(Files::isDirectory).sorted().toList()) {
          copyTree(app, "otp/" + app.getFileName().toString().replaceAll("-[0-9.]+$", "") + "/", true);
        }
      }
    }
    else {
      Path root = Paths.get(corpus);
      copyTree(root, root.getFileName() + "/", false);
      // the dependency checkouts of the corpus study (include_lib of xmpp, cowlib, hocon, ...); their files are not counted
      String deps = env("RED_DEPS");
      if (deps != null) {
        int own = myErl.size();
        try (Stream<Path> s = Files.list(Paths.get(deps))) {
          for (Path d : s.filter(Files::isDirectory).sorted().toList()) copyTree(d, "deps/" + d.getFileName() + "/", false);
        }
        myErl.subList(own, myErl.size()).clear();
      }
    }
    // an application root without an .app or .app.src (mix, erlang.mk, a .app.src.script): a generated .app.src, as a
    // build leaves an .app in ebin; master's include and include_lib resolution finds applications by those files
    int synthesized = 0;
    for (String root : myRebarConfigs) {
      String name = root.substring(root.lastIndexOf('/') + 1);
      if (myFixture.findFileInTempDir(root + "/src/" + name + ".app.src") != null ||
          myFixture.findFileInTempDir(root + "/ebin/" + name + ".app") != null || !name.matches("[a-z][a-z0-9_]*")) {
        continue;
      }
      myFixture.addFileToProject(root + "/src/" + name + ".app.src", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
      synthesized++;
    }
    out("copied erl=" + myErl.size() + " in " + (System.nanoTime() - t0) / 1_000_000 + " ms, synthesized .app.src: " + synthesized);
    List<String> rows = new ArrayList<>();
    rows.add("file\tline\ttext\tmessage");
    int refused = 0, needCopy = 0, errors = 0, done = 0;
    Map<String, Integer> refusals = new TreeMap<>();
    for (String rel : myErl) {
      VirtualFile vf = myFixture.findFileInTempDir(rel);
      if (vf == null) continue;
      Object[] r = inBackground(() -> {
        PsiFile file = PsiManager.getInstance(getProject()).findFile(vf);
        if (!(file instanceof ErlangFile ef)) return new Object[]{"not-erlang", 0, List.<String>of()};
        ErlangMacroLayer.Table t = ErlangMacroLayer.table(ef);
        if (t == null) return new Object[]{"no-table", 0, List.<String>of()};
        if (t.refusal != null) return new Object[]{t.refusal + " " + unresolvedIncludes(ef), 0, List.<String>of()};
        int need = 0;
        List<String> found = new ArrayList<>();
        for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
          if (ErlangMacroLayer.formNeedsCopy(t, c)) need++;
        }
        List<ErlangMacroLayerExpansionErrors.Problem> problems = ErlangMacroLayerExpansionErrors.problems(ef);
        if (problems == null) return new Object[]{"no-copy", need, found};
        String text = file.getText();
        for (ErlangMacroLayerExpansionErrors.Problem p : problems) {
          int line = com.intellij.openapi.util.text.StringUtil.offsetToLineNumber(text, p.range().getStartOffset()) + 1;
          found.add(rel + "\t" + line + "\t" + p.range().subSequence(text).toString().replace('\n', ' ').replace('\t', ' ') + "\t" + p.message());
        }
        return new Object[]{null, need, found};
      });
      if (r[0] != null) {
        refused++;
        refusals.merge(((String)r[0]).replaceAll("[: ].*", ""), 1, Integer::sum);
        rows.add(rel + "\t-\t-\tREFUSED " + r[0]);
      }
      if ((Integer)r[1] > 0) needCopy++;
      @SuppressWarnings("unchecked") List<String> found = (List<String>)r[2];
      for (String f : found) out("ERROR " + f);
      errors += found.size();
      rows.addAll(found);
      if (++done % 200 == 0) {
        out("progress " + done + "/" + myErl.size() + " errors=" + errors);
        PsiManager.getInstance(getProject()).dropPsiCaches();
      }
    }
    rows.add(0, "#summary\t" + corpus + "\terl=" + myErl.size() + "\trefused=" + refused + "\tneed-copy=" + needCopy + "\tfalse-reds=" + errors);
    out("corpus=" + corpus + " erl=" + myErl.size() + " refused=" + refused + " " + refusals + " files with a form needing the copy=" + needCopy +
        " false reds=" + errors + " time=" + (System.nanoTime() - t0) / 1_000_000 + " ms");
    String o = env("RED_OUT");
    if (o != null) Files.write(Paths.get(o), rows, StandardCharsets.UTF_8);
  }
}
