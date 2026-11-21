package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.psi.PsiFile;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * The minimiser: delta debugging on the text of a failing case while a failure whose signature contains
 * {@code MACRO_FUZZ_MIN_SIG} still appears. The case comes from a case file the recorder wrote
 * ({@code MACRO_FUZZ_MIN_CASE}); the main file is {@code MACRO_FUZZ_MIN_MAIN} or the first {@code .erl} of the case. The
 * check of a candidate runs the warm-up, highlighting, every read-only editor action and the target probe at the macro
 * offsets and a fixed sample of the others. First whole lines are removed, then tokens. The result is written next to
 * the case file ({@code <case>.min/}) and printed. Without {@code MACRO_FUZZ_MIN_CASE} the test does nothing.
 * <p>
 * Run: {@code MACRO_FUZZ_MIN_CASE=build/macro-fuzz/.../cases/abc-0.txt MACRO_FUZZ_MIN_SIG='isNameMadeByMacro'
 * ./gradlew --no-daemon test --rerun --tests 'org.intellij.erlang.macrolayer.fuzz.MacroFuzzMinimizeTest'}
 */
public class MacroFuzzMinimizeTest extends MacroFuzzTestBase {
  private int myTrials;
  private int myMaxTrials;

  public void testMinimize() throws Throwable {
    String casePath = FuzzConfig.env("MACRO_FUZZ_MIN_CASE");
    String sig = FuzzConfig.env("MACRO_FUZZ_MIN_SIG");
    if (casePath == null || sig == null) return;
    myMaxTrials = Integer.parseInt(Objects.requireNonNullElse(FuzzConfig.env("MACRO_FUZZ_MIN_TRIALS"), "300"));
    Map<String, String> files = readCase(Paths.get(casePath));
    String main = FuzzConfig.env("MACRO_FUZZ_MIN_MAIN");
    if (main == null) main = files.keySet().stream().filter(p -> p.endsWith(".erl")).findFirst().orElse(files.keySet().iterator().next());
    String mainFile = main;
    assertTrue("the case does not fail with '" + sig + "' before minimising", fails(files, mainFile, sig));
    // every file: lines, then tokens; the main file last so that headers shrink first
    List<String> order = new ArrayList<>(files.keySet());
    order.remove(mainFile);
    order.add(mainFile);
    for (String path : order) {
      for (boolean tokens : new boolean[]{false, true}) {
        List<String> units = split(files.get(path), tokens);
        List<String> min = ddmin(units, candidate -> {
          Map<String, String> f = new LinkedHashMap<>(files);
          f.put(path, String.join("", candidate));
          return fails(f, mainFile, sig);
        });
        files.put(path, String.join("", min));
      }
    }
    // files that are no longer needed
    for (String path : new ArrayList<>(files.keySet())) {
      if (path.equals(mainFile)) continue;
      Map<String, String> f = new LinkedHashMap<>(files);
      f.remove(path);
      if (fails(f, mainFile, sig)) files.remove(path);
    }
    Path out = Paths.get(casePath + ".min");
    Files.createDirectories(out);
    StringBuilder report = new StringBuilder("minimal case for '" + sig + "' after " + myTrials + " trials, main " + mainFile + "\n");
    for (Map.Entry<String, String> e : files.entrySet()) {
      Path p = out.resolve(e.getKey());
      Files.createDirectories(p.getParent());
      Files.writeString(p, e.getValue(), StandardCharsets.UTF_8);
      report.append("==== ").append(e.getKey()).append(" ====\n").append(e.getValue()).append('\n');
    }
    Files.writeString(out.resolve("README.txt"), report.toString(), StandardCharsets.UTF_8);
    System.err.println("MACRO-FUZZ minimised: " + report);
  }

  static Map<String, String> readCase(Path p) throws Exception {
    Map<String, String> files = new LinkedHashMap<>();
    String cur = null;
    StringBuilder sb = null;
    for (String line : Files.readAllLines(p, StandardCharsets.UTF_8)) {
      if (line.startsWith("==== file ") && line.endsWith(" ====")) {
        cur = line.substring(10, line.length() - 5);
        sb = new StringBuilder();
      }
      else if (line.equals("==== end ====") && cur != null) {
        String text = sb.toString();
        files.put(cur, text.endsWith("\n") ? text.substring(0, text.length() - 1) : text);
        cur = null;
      }
      else if (cur != null) {
        sb.append(line).append('\n');
      }
    }
    return files;
  }

  private static List<String> split(String text, boolean tokens) {
    List<String> r = new ArrayList<>();
    if (!tokens) {
      int start = 0;
      for (int i = 0; i < text.length(); i++) {
        if (text.charAt(i) == '\n') {
          r.add(text.substring(start, i + 1));
          start = i + 1;
        }
      }
      if (start < text.length()) r.add(text.substring(start));
      return r;
    }
    // tokens: runs of word characters, runs of blanks, single other characters
    int i = 0;
    while (i < text.length()) {
      int j = i + 1;
      char c = text.charAt(i);
      if (Character.isLetterOrDigit(c) || c == '_') while (j < text.length() && (Character.isLetterOrDigit(text.charAt(j)) || text.charAt(j) == '_')) j++;
      else if (c == ' ' || c == '\t') while (j < text.length() && (text.charAt(j) == ' ' || text.charAt(j) == '\t')) j++;
      r.add(text.substring(i, j));
      i = j;
    }
    return r;
  }

  interface Check {
    boolean fails(List<String> candidate) throws Throwable;
  }

  /** Zeller's ddmin, on complements only. */
  private List<String> ddmin(List<String> units, Check check) throws Throwable {
    int n = 2;
    while (units.size() >= 2 && myTrials < myMaxTrials) {
      int chunk = (int)Math.ceil(units.size() / (double)n);
      boolean reduced = false;
      for (int start = 0; start < units.size() && myTrials < myMaxTrials; start += chunk) {
        List<String> complement = new ArrayList<>(units.subList(0, start));
        complement.addAll(units.subList(Math.min(units.size(), start + chunk), units.size()));
        if (complement.isEmpty()) continue;
        if (check.fails(complement)) {
          units = complement;
          n = Math.max(n - 1, 2);
          reduced = true;
          break;
        }
      }
      if (!reduced) {
        if (n >= units.size()) break;
        n = Math.min(units.size(), n * 2);
      }
    }
    return units;
  }

  private boolean fails(Map<String, String> files, String main, String sig) throws Throwable {
    myTrials++;
    FuzzRecorder saved = rec;
    FuzzRecorder trial = new FuzzRecorder(getClass().getName() + "#trial", false);
    rec = trial;
    try {
      trial.runAll(() -> {
        trial.startCase("min:" + myTrials, 0, files);
        FuzzWorld w = world(files, false);
        try {
          PsiFile f = w.psi(main);
          if (f == null) return;
          open(f.getVirtualFile());
          warm(myFixture.getFile());
          EditorActionProbe probe = new EditorActionProbe(trial, myFixture);
          Random rnd = new Random(1);
          probe.run(probe.acts().stream().filter(a -> a.name().equals("highlight")).findFirst().orElseThrow(), rnd, "");
          TargetProbe targets = new TargetProbe(trial, myFixture.getEditor(), myFixture.getFile());
          for (int offset : TargetProbe.offsets(myFixture.getFile(), rnd, 20)) {
            myFixture.getEditor().getCaretModel().moveToOffset(Math.min(offset, myFixture.getEditor().getDocument().getTextLength()));
            for (EditorActionProbe.Act a : probe.acts()) {
              if (a.readOnly() && !a.name().equals("highlight") && !a.name().equals("goto-symbol")) probe.run(a, rnd, "@" + offset + " ");
            }
            targets.probeOffset(offset, false);
            targets.probeOffset(offset, true);
            if (hasSig(trial, sig)) return;
          }
        }
        finally {
          w.dispose();
        }
      });
    }
    finally {
      rec = saved;
    }
    return hasSig(trial, sig);
  }

  private static boolean hasSig(FuzzRecorder r, String sig) {
    for (String s : r.findings().keySet()) if (s.contains(sig)) return true;
    return false;
  }
}
