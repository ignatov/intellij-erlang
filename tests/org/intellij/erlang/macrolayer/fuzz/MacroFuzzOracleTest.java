package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubTree;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.indexing.ID;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * Oracles against the outside: (1) the layer leaves the main tree, the stubs and the file indices as they are (the same
 * with the layer on and off); (2) a module that {@code erlc} compiles gets no error highlighting; (3) a rename of a macro
 * of such a module gives a module that still compiles. (2) and (3) need erlc ({@code MACRO_FUZZ_ERLC}, default
 * {@code /opt/homebrew/bin/erlc}) and run only with {@code MACRO_FUZZ_ERLC_ORACLE=1} or in long runs.
 */
public class MacroFuzzOracleTest extends MacroFuzzTestBase {
  private static final String[] INDICES = {"ErlangModuleIndex", "ErlangApplicationIndex", "erlang.atom.index"};

  private String snapshot(PsiFile f) {
    StringBuilder sb = new StringBuilder(DebugUtil.psiToString(f, true, false));
    if (f instanceof PsiFileImpl impl) {
      StubTree tree = impl.calcStubTree();
      sb.append("\n--- stubs\n").append(DebugUtil.stubTreeToString(tree.getRoot()));
    }
    for (String id : INDICES) {
      ID<?, ?> indexId = ID.findByName(id);
      if (indexId == null) continue;
      Map<?, ?> data = FileBasedIndex.getInstance().getFileData(indexId, f.getVirtualFile(), getProject());
      sb.append("\n--- ").append(id).append(' ').append(new TreeMap<>(stringKeys(data)));
    }
    return sb.toString();
  }

  private static Map<String, String> stringKeys(Map<?, ?> m) {
    Map<String, String> r = new HashMap<>();
    for (Map.Entry<?, ?> e : m.entrySet()) r.put(String.valueOf(e.getKey()), String.valueOf(e.getValue()));
    return r;
  }

  public void testLayerKeepsTreesStubsAndIndices() throws Throwable {
    if (!LayerHooks.present()) return;
    runCases("on-off", 3, (index, rnd, caseId) -> {
      Map<String, String> files = new MacroCaseGenerator(rnd).project();
      Map<String, String> on = new LinkedHashMap<>();
      Map<String, String> off = new LinkedHashMap<>();
      for (boolean layer : new boolean[]{true, false}) {
        Disposable d = Disposer.newDisposable();
        LayerHooks.setEnabled(layer, d);
        FuzzWorld w = world(files, false);
        try {
          for (String p : w.erlangPaths()) {
            PsiFile f = w.psi(p);
            if (f == null) continue;
            if (p.equals("m.erl")) {
              open(f.getVirtualFile());
              warm(f);
              rec.guard("highlight layer=" + layer, () -> myFixture.doHighlighting());
            }
            String[] s = new String[1];
            com.intellij.testFramework.IndexingTestUtil.waitUntilIndexesAreReady(getProject());
            rec.guard("snapshot " + p + " layer=" + layer, () -> s[0] = snapshot(f));
            if (s[0] != null) (layer ? on : off).put(p, s[0]);
          }
        }
        finally {
          w.dispose();
          Disposer.dispose(d);
        }
      }
      for (String p : on.keySet()) {
        if (off.containsKey(p) && !Objects.equals(on.get(p), off.get(p))) {
          rec.fail("layer-changes-tree", "tree, stubs or indices differ with the layer on and off", p + ": " + firstDiff(on.get(p), off.get(p)));
        }
      }
    });
  }

  private static String firstDiff(String a, String b) {
    if (a == null || b == null) return a + " / " + b;
    int i = 0;
    while (i < a.length() && i < b.length() && a.charAt(i) == b.charAt(i)) i++;
    return "at " + i + ": on «" + a.substring(Math.max(0, i - 80), Math.min(a.length(), i + 80)) + "» off «" +
           b.substring(Math.max(0, i - 80), Math.min(b.length(), i + 80)) + "»";
  }

  /** Compiles the files with erlc in a scratch folder; returns null when they compile, else erlc's output. */
  static String erlc(Map<String, String> files, String main) throws Exception {
    String erlc = Objects.requireNonNullElse(FuzzConfig.env("MACRO_FUZZ_ERLC"), "/opt/homebrew/bin/erlc");
    Path dir = FuzzConfig.out().resolve("erlc-work").resolve(Integer.toHexString(files.hashCode()));
    for (Map.Entry<String, String> e : files.entrySet()) {
      Path p = dir.resolve(e.getKey());
      Files.createDirectories(p.getParent());
      Files.writeString(p, e.getValue(), StandardCharsets.UTF_8);
    }
    Files.createDirectories(dir.resolve("ebin"));
    Process p = new ProcessBuilder(erlc, "-o", dir.resolve("ebin").toString(), "-I", dir.toString(), "-I", dir.resolve("include").toString(),
                                   dir.resolve(main).toString()).directory(dir.toFile()).redirectErrorStream(true).start();
    String out = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
    if (!p.waitFor(60, TimeUnit.SECONDS)) {
      p.destroyForcibly();
      return "timeout";
    }
    return p.exitValue() == 0 ? null : out;
  }

  private static boolean erlcOn() {
    String erlc = Objects.requireNonNullElse(FuzzConfig.env("MACRO_FUZZ_ERLC"), "/opt/homebrew/bin/erlc");
    return Files.isExecutable(Paths.get(erlc)) && (FuzzConfig.env("MACRO_FUZZ_ERLC_ORACLE") != null || FuzzConfig.isLongRun());
  }

  public void testErlcCleanModulesHaveNoErrorsAndRenameKeepsThemCompiling() throws Throwable {
    if (!erlcOn()) return;
    EditorActionProbe.enableInspections(myFixture);
    Map<String, String> samples = FuzzInputs.samples();
    List<String> sampleFiles = new ArrayList<>();
    for (String p : samples.keySet()) if (p.endsWith(".erl") && !p.contains("/errors/") && !p.contains("/otp/")) sampleFiles.add(p);
    runCases("erlc", 10, (index, rnd, caseId) -> {
      Map<String, String> files;
      String main;
      if (index % 2 == 1 && !sampleFiles.isEmpty()) {
        files = samples;
        main = sampleFiles.get(index / 2 % sampleFiles.size());
      }
      else {
        files = new MacroCaseGenerator(rnd).project();
        main = "m.erl";
      }
      String err = erlc(files, main);
      if (err != null) return;                                  // only modules erlc compiles
      FuzzWorld w = world(files, false);
      w.addIncludeRoot("include");
      try {
        PsiFile f = w.psi(main);
        open(f.getVirtualFile());
        warm(f);
        List<HighlightInfo>[] infos = new List[1];
        rec.guard("highlight", () -> infos[0] = myFixture.doHighlighting(HighlightSeverity.ERROR));
        if (infos[0] != null) {
          for (HighlightInfo h : infos[0]) {
            if (h.getSeverity() != HighlightSeverity.ERROR) continue;
            String at = myFixture.getEditor().getDocument().getText().substring(h.getStartOffset(), Math.min(h.getEndOffset(), h.getStartOffset() + 40));
            rec.fail("false-error", "error in a module erlc compiles: " + String.valueOf(h.getDescription()).replaceAll("'[^']*'", "'X'"),
                     main + " at " + h.getStartOffset() + " «" + at + "»: " + h.getDescription());
          }
        }
        // rename a macro definition of the main module to a fresh name, then compile again
        PsiFile file = myFixture.getFile();
        List<com.intellij.psi.PsiElement> defs = new ArrayList<>(com.intellij.psi.util.PsiTreeUtil.findChildrenOfType(file,
          org.intellij.erlang.psi.ErlangMacrosDefinition.class));
        if (defs.isEmpty()) return;
        com.intellij.psi.PsiElement def = defs.get(rnd.nextInt(defs.size()));
        org.intellij.erlang.psi.ErlangMacrosName name = ((org.intellij.erlang.psi.ErlangMacrosDefinition)def).getMacrosName();
        if (name == null) return;
        String old = name.getText();
        String fresh = "FUZZ_RENAMED_" + Math.abs(rnd.nextInt(1000));
        boolean ok = rec.guard("rename macro " + old + " -> " + fresh, () -> myFixture.renameElement(def, fresh));
        if (!ok) return;
        Map<String, String> after = new LinkedHashMap<>(files);
        for (String p : w.erlangPaths()) {
          PsiFile pf = w.psi(p);
          if (pf != null) after.put(p, com.intellij.psi.PsiDocumentManager.getInstance(getProject()).getDocument(pf).getText());
        }
        rec.setCaseFiles(after);
        String err2 = erlc(after, main);
        if (err2 != null) {
          rec.fail("rename-breaks-compile", "a macro rename gives a module erlc refuses", main + " " + old + " -> " + fresh + ": " +
                                                                                         err2.substring(0, Math.min(400, err2.length())));
        }
      }
      finally {
        w.dispose();
      }
    });
  }
}
