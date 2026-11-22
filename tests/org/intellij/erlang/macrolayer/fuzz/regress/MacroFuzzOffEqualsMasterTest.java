package org.intellij.erlang.macrolayer.fuzz.regress;

import org.intellij.erlang.macrolayer.fuzz.FuzzConfig;
import org.intellij.erlang.macrolayer.fuzz.FuzzInputs;
import org.intellij.erlang.macrolayer.fuzz.LayerHooks;
import org.intellij.erlang.macrolayer.fuzz.MacroFuzzAnswersTest;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The acceptance test of the layer's off switch (finding F4): with the layer off, the answers of the branch for the
 * sample files (highlighting, references, completion at the same seeded offsets) must equal master's, stored in
 * {@code testData/macroFuzz/off-equals-master/answers_<case>.txt} (dumped on master 2a6160fc). Accepted differences: a
 * reference whose targets include master's (every definition in conditional branches, D2), and the lines of
 * {@code allowed.txt} (D3 cases where epp agrees with the branch, and the F5 rule for directive names).
 */
public class MacroFuzzOffEqualsMasterTest extends MacroFuzzAnswersTest {
  private static final Path GOLDEN = Paths.get("testData", "macroFuzz", "off-equals-master");
  private static final Pattern REF = Pattern.compile("^(\\d+ \\S*) -> \\[(.*)]$");

  @Override
  public void testDumpAnswers() {
    // the dump of the parent class is not part of this test
  }

  public void testLayerOffEqualsMaster() throws Throwable {
    LayerHooks.setEnabled(false, getTestRootDisposable());
    Set<String> allowed = new HashSet<>();
    for (String l : Files.readAllLines(GOLDEN.resolve("allowed.txt"), StandardCharsets.UTF_8)) {
      if (!l.isBlank() && !l.startsWith("#")) allowed.add(l.trim());
    }
    Map<String, String> samples = FuzzInputs.samples();
    List<String> sampleFiles = new ArrayList<>();
    for (String p : samples.keySet()) if (p.endsWith(".erl")) sampleFiles.add(p);
    List<String> problems = new ArrayList<>();
    org.intellij.erlang.macrolayer.fuzz.EditorActionProbe.enableInspections(myFixture);
    rec = new org.intellij.erlang.macrolayer.fuzz.FuzzRecorder(getClass().getName() + "#" + getName());
    rec.runAll(() -> {
      try (var stream = Files.list(GOLDEN)) {
        for (Path g : stream.filter(p -> p.getFileName().toString().startsWith("answers_")).sorted().toList()) {
          String name = g.getFileName().toString();
          int index = Integer.parseInt(name.replaceAll("\\D", ""));
          long seed = FuzzConfig.caseSeed(FuzzConfig.DEFAULT_SEED, "answers", index);
          String branch = answersOf(index, new Random(seed), "answers:" + index, samples, sampleFiles);
          problems.addAll(compare(name, Files.readString(g, StandardCharsets.UTF_8), branch, allowed));
        }
      }
    });
    assertTrue(String.join("\n", problems.subList(0, Math.min(40, problems.size()))) + "\n(" + problems.size() + " differences)",
               problems.isEmpty());
  }

  private static Map<String, Set<String>> sections(String dump) {
    Map<String, Set<String>> r = new HashMap<>();
    String sec = "";
    for (String l : dump.split("\n")) {
      if (l.startsWith("# ")) continue;
      if (l.startsWith("## ")) {
        sec = l.substring(3);
        continue;
      }
      r.computeIfAbsent(sec, k -> new TreeSet<>()).add(l.replace("-> [null]", "-> []"));
    }
    return r;
  }

  static List<String> compare(String name, String master, String branch, Set<String> allowed) {
    Map<String, Set<String>> m = sections(master);
    Map<String, Set<String>> b = sections(branch);
    List<String> problems = new ArrayList<>();
    for (String sec : List.of("highlights", "completion")) {
      Set<String> ms = m.getOrDefault(sec, Set.of());
      Set<String> bs = b.getOrDefault(sec, Set.of());
      for (String l : bs) if (!ms.contains(l)) problems.add(name + " " + sec + " only on the branch: " + l);
      for (String l : ms) if (!bs.contains(l)) problems.add(name + " " + sec + " only on master: " + l);
    }
    // references by offset and class; the branch may add targets to master's (D2)
    Map<String, Set<String>> mr = refs(m.getOrDefault("references", Set.of()));
    Map<String, Set<String>> br = refs(b.getOrDefault("references", Set.of()));
    for (String key : new TreeSet<>(union(mr.keySet(), br.keySet()))) {
      Set<String> mt = mr.get(key);
      Set<String> bt = br.get(key);
      if (Objects.equals(mt, bt)) continue;
      if (allowed.contains(name + "|" + key)) continue;
      if (mt != null && bt != null && bt.containsAll(mt) && !mt.isEmpty()) continue;
      problems.add(name + " reference " + key + ": branch " + bt + ", master " + mt);
    }
    return problems;
  }

  private static Set<String> union(Set<String> a, Set<String> b) {
    Set<String> r = new HashSet<>(a);
    r.addAll(b);
    return r;
  }

  private static Map<String, Set<String>> refs(Set<String> lines) {
    Map<String, Set<String>> r = new HashMap<>();
    for (String l : lines) {
      Matcher mm = REF.matcher(l);
      if (!mm.matches()) continue;
      Set<String> t = r.computeIfAbsent(mm.group(1), k -> new TreeSet<>());
      for (String x : mm.group(2).split(", ")) if (!x.isEmpty()) t.add(x);
    }
    return r;
  }
}
