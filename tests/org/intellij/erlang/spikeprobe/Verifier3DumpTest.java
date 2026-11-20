package org.intellij.erlang.spikeprobe;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubTree;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.indexing.ID;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.*;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;
import static org.intellij.erlang.spikeprobe.Verifier3Support.counter;

/** Verifier 3: per file, hashes of the PSI tree, stub tree and index keys, and every highlight info with all inspections. */
public class Verifier3DumpTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override protected LightProjectDescriptor getProjectDescriptor() { return descriptor(); }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected void runTestRunnable(com.intellij.util.ThrowableRunnable<Throwable> testRunnable) throws Throwable {
    if (env("PROBE_OUT") == null || env("V3_FILES") == null) return;
    super.runTestRunnable(testRunnable);
  }

  @SuppressWarnings("unchecked")
  private static <K> ID<K, ?> id(String name) { return (ID<K, ?>)ID.findByName(name); }

  private static String sha(String s) throws Exception {
    byte[] d = MessageDigest.getInstance("SHA-256").digest(s.getBytes(StandardCharsets.UTF_8));
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < 8; i++) sb.append(String.format("%02x", d[i]));
    return sb.toString();
  }

  public void testDump() throws Exception {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
    Path dir = Paths.get(env("PROBE_OUT"), label(), "dump");
    Files.createDirectories(dir);
    List<String[]> items = new ArrayList<>();
    for (String fd : List.of("testData/verifier3", "testData/macroSpikeProbes", "testData/verifier2")) {
      Path d = Paths.get(fd);
      if (!Files.isDirectory(d)) continue;
      try (var s = Files.list(d)) {
        s.filter(p -> p.toString().endsWith(".erl")).sorted().forEach(p -> items.add(new String[]{"fx-" + p.getFileName(), p.toString()}));
      }
    }
    for (String l : Files.readAllLines(Paths.get(env("V3_FILES")))) {
      if (l.isBlank()) continue;
      String[] w = l.split("\t");
      items.add(new String[]{w[0], w[1]});
    }
    long budget = Long.parseLong(Objects.requireNonNullElse(env("PROBE_BUDGET_SEC"), "1500")) * 1000;
    long start = System.currentTimeMillis();
    int done = 0;
    for (String[] it : items) {
      Path out = dir.resolve(it[0] + ".json");
      if (Files.exists(out)) continue;
      if (System.currentTimeMillis() - start > budget) break;
      Files.writeString(dir.resolve("_current.txt"), it[0] + "\t" + it[1]);
      Path path = Paths.get(it[1]);
      String text = read(path);
      JsonObject r = new JsonObject();
      r.addProperty("id", it[0]);
      r.addProperty("path", it[1]);
      VirtualFile vf = myFixture.getTempDirFixture().createFile("d" + it[0] + "/" + path.getFileName(), text);
      myFixture.configureFromExistingVirtualFile(vf);
      PsiFile file = myFixture.getFile();
      List<Failure> f1 = guarded(() -> ReadAction.run(() -> {
        String tree = DebugUtil.psiToString(file, true, false);
        StubTree stub = ((PsiFileImpl)file).calcStubTree();
        String stubs = DebugUtil.stubTreeToString(stub.getRoot());
        FileBasedIndex index = FileBasedIndex.getInstance();
        String mod = new TreeMap<>(index.getFileData(id("ErlangModuleIndex"), vf, getProject())).keySet().toString();
        String atoms = new TreeSet<>(index.getFileData(id("erlang.atom.index"), vf, getProject()).keySet()).toString();
        r.addProperty("tree", sha(tree));
        r.addProperty("stubs", sha(stubs));
        r.addProperty("index", sha(mod + atoms));
        if (it[0].startsWith("fx-")) {
          try { Files.writeString(dir.resolve(it[0] + ".tree.txt"), tree + "\n--- stub\n" + stubs + "\n--- index\n" + mod + "\n" + atoms); }
          catch (Exception e) { throw new RuntimeException(e); }
        }
      }));
      int b0 = counter("EXPANSIONS_BUILT"), p0 = counter("COPIES_PARSED");
      JsonArray infos = new JsonArray();
      Document doc = myFixture.getEditor().getDocument();
      long t0 = System.nanoTime();
      List<Failure> f2 = guarded(() -> {
        for (HighlightInfo info : myFixture.doHighlighting()) {
          if (info.getDescription() == null) continue;
          JsonObject o = new JsonObject();
          o.addProperty("sev", info.getSeverity().getName());
          o.addProperty("tool", info.getInspectionToolId());
          o.addProperty("desc", clip(info.getDescription(), 200));
          o.addProperty("start", info.getStartOffset());
          o.addProperty("end", info.getEndOffset());
          o.addProperty("line", doc.getLineNumber(Math.min(info.getStartOffset(), doc.getTextLength())) + 1);
          infos.add(o);
        }
      });
      r.addProperty("highlightMs", (System.nanoTime() - t0) / 1_000_000);
      r.addProperty("builds", counter("EXPANSIONS_BUILT") - b0);
      r.addProperty("parsed", counter("COPIES_PARSED") - p0);
      int b1 = counter("EXPANSIONS_BUILT");
      guarded(() -> myFixture.doHighlighting());
      r.addProperty("buildsSecondHighlight", counter("EXPANSIONS_BUILT") - b1);
      r.add("highlight", infos);
      JsonArray fails = new JsonArray();
      for (Failure x : f1) fails.add(x.json());
      for (Failure x : f2) fails.add(x.json());
      r.add("failures", fails);
      Files.writeString(out, r.toString());
      guarded(() -> WriteAction.runAndWait(() -> vf.getParent().delete(this)));
      done++;
    }
    Files.writeString(dir.resolve("_run-" + System.currentTimeMillis() + ".txt"), "done " + done + " of " + items.size());
  }
}
