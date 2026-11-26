package org.intellij.erlang.sweep;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageStructureViewBuilder;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.lang.findUsages.LanguageFindUsages;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.lang.folding.LanguageFolding;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.util.ui.UIUtil;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.ex.FileEditorManagerEx;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.TokenType;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubTextInconsistencyException;
import com.intellij.testFramework.DumbModeTestUtils;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.zip.CRC32;

/**
 * Robustness sweep: runs the plugin's IDE features (highlighting with all inspections, completion, reformat, structure
 * view, folding, resolve, find usages, rename, reparse and stub consistency, typing) over a corpus of Erlang files, seeded
 * mutations of them and synthetic stress inputs, and records every exception and logged error. It never fails on a
 * finding; it writes a JSON report, a JSONL journal and (optionally) a ledger line.
 * <p>
 * The test is a no-op unless one of the environment variables SWEEP_CORPUS, SWEEP_SYNTH, SWEEP_SCALE or SWEEP_PLANT is set,
 * so a plain {@code ./gradlew test} is not affected. See README.md next to the runner for the knobs.
 */
public class ErlangRobustnessSweepTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final List<String> ALL_OPS = List.of("stubs", "highlight", "resolve", "structure", "folding", "completion",
                                               "usages", "rename", "format", "reparse", "typing",
                                               "resolve-bg", "structure-bg", "folding-bg");
  /** Read-only operations that also run the way the IDE runs them: on a pooled thread, in a read action, under a progress indicator. */
  private static final Set<String> BACKGROUND_OPS = Set.of("resolve-bg", "structure-bg", "folding-bg");
  /** Templates and other files with an Erlang extension that are not Erlang. */
  private static final Set<String> NOT_ERLANG = Set.of("leexinc.hrl");
  private static final java.util.regex.Pattern MAIN_SET =
    java.util.regex.Pattern.compile("^(lib/[^/]+/(src|include|examples)/|erts/preloaded/src/).*");
  private static final java.util.regex.Pattern TEST_SET =
    java.util.regex.Pattern.compile("^(lib/[^/]+/test/|erts/emulator/test/|erts/test/).*");
  static final List<String> ALL_MUTATIONS = List.of("truncate", "delete-token", "dup-token", "swap-tokens", "garbage",
                                                     "bracket", "unterminated-string", "unterminated-atom");
  private static final String HARNESS_CLASS = ErlangRobustnessSweepTest.class.getName();
  private static final String BASE_CLASS = ErlangLightPlatformCodeInsightFixtureTestCase.class.getName();
  private static final Gson GSON = new GsonBuilder().disableHtmlEscaping().create();
  private static final Gson PRETTY = new GsonBuilder().disableHtmlEscaping().setPrettyPrinting().create();

  // ---- configuration
  private long mySeed;
  private String myMode;
  private List<String> myOps;
  private List<String> myMutationKinds;
  private int myMutations;
  private String myCorpusSet;
  private double myScaleRatio;
  private boolean myOriginals;
  private int myLimit;
  private int myShardIndex;
  private int myShardCount;
  private String myOnly;
  private String myPlant;
  private long mySlowMs;
  private long myHangMs;
  private long myOpBudgetMs;
  private long myInputBudgetMs;
  private int myCompletions;
  private int myPicks;
  private long myBudgetMs;
  private Path myDumpDir;
  private final Map<String, String> myKnown = new LinkedHashMap<>();

  // ---- state
  private BufferedWriter myJournal;
  private final Acc myAcc = new Acc();
  private final Set<String> myDoneInputs = new HashSet<>();
  private volatile long myOpStart;
  private volatile String myOpLabel;
  private volatile JsonObject myOpContext;
  private volatile Thread myOpThread;
  private Input myCurrent;
  private final List<VirtualFile> myCreated = new ArrayList<>();
  private Consumer<JsonObject> mySink;
  private volatile boolean myKilled;
  private long myPeakHeapMb;

  public ErlangRobustnessSweepTest() {
    super("smallide".equals(env("SWEEP_MODE")));
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return switch (sdkRelease()) {
          case "27" -> ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0);
          case "28" -> ErlangSdkType.createMockSdk("testData/mockSdk-R28/", ErlangSdkRelease.V_28_0);
          default -> ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
        };
      }
    };
  }

  /**
   * The mock SDK decides the release-gated syntax, so it follows the corpus: SWEEP_SDK_RELEASE, or the major version in
   * SWEEP_CORPUS_ID (OTP-28.1 gives 28). 27, 28 and 29 have a mock SDK; anything else gets 29.
   */
  static String sdkRelease() {
    String release = env("SWEEP_SDK_RELEASE");
    if (release == null) {
      java.util.regex.Matcher m = java.util.regex.Pattern.compile("(?i)otp[-_]?(\\d+)").matcher(env("SWEEP_CORPUS_ID", ""));
      release = m.find() ? m.group(1) : "29";
    }
    return List.of("27", "28").contains(release) ? release : "29";
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ((CodeInsightTestFixtureImpl)myFixture).canChangeDocumentDuringHighlighting(true);
  }

  static @Nullable String env(String name) {
    String v = System.getenv(name);
    if (v == null || v.isEmpty()) v = System.getProperty(name);
    return v == null || v.isEmpty() ? null : v;
  }

  private static String env(String name, String def) {
    String v = env(name);
    return v == null ? def : v;
  }

  private static List<String> csv(String s) {
    List<String> r = new ArrayList<>();
    for (String p : s.split(",")) if (!p.trim().isEmpty()) r.add(p.trim());
    return r;
  }

  // =================================================================================================== entry point

  public void testSweep() throws Exception {
    String corpus = env("SWEEP_CORPUS");
    String synth = env("SWEEP_SYNTH");
    boolean scale = env("SWEEP_SCALE") != null;
    myPlant = env("SWEEP_PLANT");
    if (corpus == null && synth == null && !scale && myPlant == null) return;

    myMode = env("SWEEP_MODE", "normal");
    if (!List.of("normal", "dumb", "smallide").contains(myMode)) throw new IllegalArgumentException("SWEEP_MODE " + myMode);
    String seedText = env("SWEEP_SEED");
    mySeed = seedText == null ? System.nanoTime() : Long.parseLong(seedText);
    List<String> defaultOps = "dumb".equals(myMode)
                              ? List.of("highlight", "resolve", "structure", "folding", "completion", "format", "typing")
                              : ALL_OPS;
    myCorpusSet = env("SWEEP_CORPUS_SET", "all");
    if (!List.of("all", "main", "test").contains(myCorpusSet)) throw new IllegalArgumentException("SWEEP_CORPUS_SET " + myCorpusSet);
    myScaleRatio = Double.parseDouble(env("SWEEP_SCALE_RATIO", "8"));
    myOps = env("SWEEP_OPS") == null ? defaultOps : csv(env("SWEEP_OPS"));
    for (String op : myOps) if (!ALL_OPS.contains(op)) throw new IllegalArgumentException("unknown op " + op);
    myMutationKinds = env("SWEEP_MUTATION_KINDS") == null ? ALL_MUTATIONS : csv(env("SWEEP_MUTATION_KINDS"));
    for (String k : myMutationKinds) if (!ALL_MUTATIONS.contains(k)) throw new IllegalArgumentException("unknown mutation " + k);
    myMutations = Integer.parseInt(env("SWEEP_MUTATIONS", "0"));
    myOriginals = !"0".equals(env("SWEEP_ORIGINALS", "1"));
    myLimit = Integer.parseInt(env("SWEEP_LIMIT", "0"));
    String shard = env("SWEEP_SHARD", "0/1");
    myShardIndex = Integer.parseInt(shard.substring(0, shard.indexOf('/')));
    myShardCount = Integer.parseInt(shard.substring(shard.indexOf('/') + 1));
    myOnly = env("SWEEP_ONLY");
    mySlowMs = Long.parseLong(env("SWEEP_SLOW_MS", myPlant != null ? "1000" : "10000"));
    // A hang halts the JVM and the rerun starts it again: in a run of the synthetic inputs only, which are small but for
    // a few stress inputs with known hangs, a shorter wait leaves the time to the other inputs.
    myHangMs = Long.parseLong(env("SWEEP_HANG_SECONDS", "only".equals(synth) ? "60" : "300")) * 1000;
    myOpBudgetMs = Long.parseLong(env("SWEEP_OP_BUDGET_MS", "30000"));
    myInputBudgetMs = Long.parseLong(env("SWEEP_INPUT_BUDGET_SECONDS", "120")) * 1000;
    myCompletions = Integer.parseInt(env("SWEEP_COMPLETIONS", "3"));
    myPicks = Integer.parseInt(env("SWEEP_PICKS", "3"));
    myBudgetMs = Long.parseLong(env("SWEEP_BUDGET_MINUTES", "0")) * 60_000;
    Path report = Paths.get(env("SWEEP_REPORT", "build/sweep/report.json")).toAbsolutePath();
    Path journal = Paths.get(env("SWEEP_JOURNAL", report + ".journal.jsonl")).toAbsolutePath();
    myDumpDir = env("SWEEP_DUMP_DIR") == null ? null : Paths.get(env("SWEEP_DUMP_DIR")).toAbsolutePath();
    Files.createDirectories(report.getParent());
    Files.createDirectories(journal.getParent());
    loadKnown(env("SWEEP_KNOWN"));

    boolean resume = env("SWEEP_RESUME") != null && Files.exists(journal);
    if (resume) replay(journal);
    myJournal = Files.newBufferedWriter(journal, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
                                        resume ? StandardOpenOption.APPEND : StandardOpenOption.TRUNCATE_EXISTING);
    mySink = rec -> {
      if (myKilled) return;
      journal(rec);
      myAcc.accept(rec);
    };
    Thread killHook = new Thread(() -> {
      // SIGTERM (a cancelled CI job, a Gradle timeout): the application is disposed under the running operation and
      // everything fails with "VFS is disconnected". Mark the journal and stop recording.
      synchronized (this) {
        try {
          myJournal.write("{\"t\":\"killed\"}\n");
          myJournal.flush();
        }
        catch (IOException ignored) {
        }
        myKilled = true;
      }
    }, "sweep-kill-hook");
    Runtime.getRuntime().addShutdownHook(killHook);

    Path root = corpus == null ? null : Paths.get(corpus).toRealPath();
    String revision = revision();
    JsonObject config = new JsonObject();
    config.addProperty("seed", mySeed);
    config.addProperty("revision", revision);
    config.addProperty("corpus", root == null ? null : root.toString());
    config.addProperty("corpusId", env("SWEEP_CORPUS_ID", root == null ? "none" : root.getFileName().toString()));
    config.addProperty("corpusSet", myCorpusSet);
    config.addProperty("sdkRelease", sdkRelease());
    config.addProperty("mode", myMode);
    config.add("ops", GSON.toJsonTree(myOps));
    config.addProperty("originals", myOriginals);
    config.addProperty("mutations", myMutations);
    config.add("mutationKinds", GSON.toJsonTree(myMutations > 0 ? myMutationKinds : List.of()));
    config.addProperty("synthetic", synth);
    config.addProperty("scale", scale);
    config.addProperty("plant", myPlant);
    config.addProperty("shard", myShardIndex + "/" + myShardCount);
    config.addProperty("limit", myLimit);
    config.addProperty("only", myOnly);
    config.addProperty("completions", myCompletions);
    config.addProperty("picks", myPicks);
    config.addProperty("slowMs", mySlowMs);
    config.addProperty("opBudgetMs", myOpBudgetMs);
    config.addProperty("inputBudgetMs", myInputBudgetMs);
    config.addProperty("hangMs", myHangMs);
    config.addProperty("budgetMinutes", myBudgetMs / 60_000);
    config.addProperty("maxHeapMb", Runtime.getRuntime().maxMemory() / (1024 * 1024));
    config.addProperty("java", System.getProperty("java.version"));
    config.addProperty("os", System.getProperty("os.name"));
    config.addProperty("date", Instant.now().toString());
    config.addProperty("resumed", resume);
    JsonObject run = rec("run");
    run.add("config", config);
    journal(run);
    myAcc.accept(run);
    System.out.println("SWEEP: " + GSON.toJson(config));

    if ("dumb".equals(myMode)) CodeInsightTestFixtureImpl.mustWaitForSmartMode(false, getTestRootDisposable());
    enableInspections();
    if (myPlant != null) SweepPlants.install(myFixture, getTestRootDisposable());
    Thread watchdog = startWatchdog();

    long start = System.currentTimeMillis();
    boolean complete = true;
    int files = 0;
    try {
      if (myPlant != null) {
        for (Input in : SweepPlants.inputs()) if (!skip(in.id)) runInput(in, mySink);
        if ("hang".equals(myPlant) && !skip("plant/plant_hang.erl")) runInput(new Input("plant/plant_hang.erl", "plant_hang.erl", "-module(plant_hang).\nplant_hang() -> ok.\n"), mySink);
        for (JsonObject c : SweepPlants.controls(myMode, myOps, myAcc)) mySink.accept(c);
      }
      if (synth != null) {
        for (Input in : Synthetic.inputs(mySeed, this::tokens)) {
          if (budgetExceeded(start)) {
            complete = false;
            break;
          }
          if (skip(in.id)) continue;
          runInput(in, mySink);
        }
      }
      // After the synthetic inputs: the probe is slow and has found only known problems, so a budget stop cuts it first.
      if (scale && complete && !scalingProbe(start)) complete = false;
      if (root != null && !"only".equals(synth)) {
        List<Path> selected = selectFiles(root);
        files = selected.size();
        int n = 0;
        outer:
        for (Path file : selected) {
          String rel = root.relativize(file).toString().replace('\\', '/');
          byte[] bytes = Files.readAllBytes(file);
          String name = file.getFileName().toString();
          if (++n % 50 == 0) {
            Runtime rt = Runtime.getRuntime();
            System.out.println("SWEEP: " + n + "/" + files + " usedHeapMb=" + (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024) +
                               " elapsedS=" + (System.currentTimeMillis() - start) / 1000);
          }
          if (myOriginals && !skip(rel + "#orig")) {
            if (budgetExceeded(start)) {
              complete = false;
              break;
            }
            runInput(new Input(rel + "#orig", name, bytes), mySink);
          }
          for (int m = 0; m < myMutations; m++) {
            String id = rel + "#m" + m;
            if (skip(id)) continue;
            if (budgetExceeded(start)) {
              complete = false;
              break outer;
            }
            Input in = mutate(rel, name, bytes, m);
            if (in != null) runInput(in, mySink);
          }
        }
      }
      String minimize = env("SWEEP_MINIMIZE");
      if (minimize != null && root != null) minimize(root, minimize);
    }
    finally {
      watchdog.interrupt();
      try {
        Runtime.getRuntime().removeShutdownHook(killHook);
      }
      catch (IllegalStateException ignored) {
      }
    }
    if (myKilled) return;

    long wall = System.currentTimeMillis() - start;
    JsonObject end = rec("end");
    end.addProperty("complete", complete);
    end.addProperty("wallMs", wall);
    journal(end);
    myJournal.close();

    JsonObject reportJson = myAcc.report(config, files, wall, complete, myKnown);
    // Used heap after an input, garbage included: an upper bound of what the run needed.
    reportJson.getAsJsonObject("totals").addProperty("peakUsedHeapMb", myPeakHeapMb);
    reportJson.add("unproven_detectors", GSON.toJsonTree(UNPROVEN_DETECTORS));
    Files.writeString(report, PRETTY.toJson(reportJson) + "\n");
    String ledger = env("SWEEP_LEDGER");
    if (ledger != null) {
      Path ledgerPath = Paths.get(ledger).toAbsolutePath();
      if (ledgerPath.getParent() != null) Files.createDirectories(ledgerPath.getParent());
      JsonObject line = myAcc.ledger(config, files, wall, complete, myKnown);
      JsonObject credit = new JsonObject();
      JsonObject reasons = new JsonObject();
      for (String op : myOps) {
        String reason = noCredit(myMode, op);
        credit.addProperty(op, reason == null);
        if (reason != null) reasons.addProperty(op, reason);
      }
      line.add("credit", credit);
      line.add("credit_reasons", reasons);
      line.add("unproven_detectors", GSON.toJsonTree(UNPROVEN_DETECTORS));
      line.getAsJsonObject("totals").addProperty("peakUsedHeapMb", myPeakHeapMb);
      Files.writeString(ledgerPath, GSON.toJson(line) + "\n",
                        StandardOpenOption.CREATE, StandardOpenOption.APPEND);
    }
    System.out.println("SWEEP: done " + GSON.toJson(reportJson.get("totals")) + " report=" + report);
    if (myPlant != null && !"hang".equals(myPlant)) {
      List<String> missed = new ArrayList<>();
      for (JsonObject c : myAcc.controls) {
        boolean required = !c.has("required") || c.get("required").getAsBoolean();
        if (required && !c.get("fired").getAsBoolean()) missed.add(c.get("name").getAsString());
      }
      assertTrue("Positive controls that did not fire: " + missed, missed.isEmpty());
    }
  }

  /**
   * Detectors that no positive control has shown firing. A clean run says nothing about them, and the ledger record
   * carries the list so that a scheduler does not count it.
   */
  static final List<String> UNPROVEN_DETECTORS = List.of(
    "PsiTextMismatch: no control makes the PSI/text check fail",
    "ReadAccess: the harness supplies the read action (EDT or ReadAction), so plugin code that needs one and has none is not exposed; the control proves the classification only",
    "SlowOps: the platform assertion did not fire on the EDT in the test application",
    "TreeAccess: no control",
    "OutOfMemory halt: no control",
    "IndexNotReady in dumb mode inside a folding builder or a completion contributor: the platform swallows it (inside an inspection it is logged and detected)");

  /** The reason why a clean run of the operation in the mode proves nothing, or null. */
  static @Nullable String noCredit(String mode, String op) {
    if (!mode.equals("dumb")) return null;
    return switch (op) {
      case "folding", "completion" ->
        "an IndexNotReadyException thrown by the plugin's code in this operation reaches no detector in dumb mode";
      default -> null;
    };
  }

  private boolean skip(String id) {
    if (myDoneInputs.contains(id)) return true;
    if (myOnly == null) return false;
    for (String part : csv(myOnly)) if (id.contains(part)) return false;
    return true;
  }

  private boolean budgetExceeded(long start) {
    return myBudgetMs > 0 && System.currentTimeMillis() - start > myBudgetMs;
  }

  private List<Path> selectFiles(Path root) throws IOException {
    List<Path> files;
    try (Stream<Path> s = Files.walk(root)) {
      files = s.filter(p -> {
        String n = p.getFileName().toString();
        return Files.isRegularFile(p) && (n.endsWith(".erl") || n.endsWith(".hrl"));
      }).sorted().toList();
    }
    List<Path> selected = new ArrayList<>();
    Map<Path, Long> order = new java.util.HashMap<>();
    for (Path p : files) {
      String rel = root.relativize(p).toString().replace('\\', '/');
      // The shard is a function of the path only, so adding or removing files does not move the others.
      if (NOT_ERLANG.contains(p.getFileName().toString())) continue;
      if (myCorpusSet.equals("main") && !MAIN_SET.matcher(shardKey(rel)).matches()) continue;
      if (myCorpusSet.equals("test") && !TEST_SET.matcher(shardKey(rel)).matches()) continue;
      if (Math.floorMod(stableHash(shardKey(rel)), myShardCount) != myShardIndex) continue;
      selected.add(p);
      order.put(p, new Random(mySeed * 31 + stableHash(rel + "\u0000order")).nextLong());
    }
    // The order inside the shard depends on the seed, so a run that stops on its time budget covers another part of
    // the shard next time. The inputs themselves do not depend on the order.
    selected.sort((a, b) -> {
      int c = Long.compare(order.get(a), order.get(b));
      return c != 0 ? c : a.compareTo(b);
    });
    return myLimit > 0 && selected.size() > myLimit ? new ArrayList<>(selected.subList(0, myLimit)) : selected;
  }

  /** Path without the version suffix of the OTP application directory, so that a new OTP patch keeps the shards. */
  static String shardKey(String rel) {
    return rel.replaceAll("(^|/)lib/([a-z_0-9]+)-[0-9.]+/", "$1lib/$2/");
  }

  static int stableHash(String s) {
    CRC32 crc = new CRC32();
    crc.update(s.getBytes(StandardCharsets.UTF_8));
    return (int)crc.getValue();
  }

  private static String revision() {
    String r = env("SWEEP_REVISION");
    if (r == null) r = env("GITHUB_SHA");
    if (r != null) return r;
    try {
      Process p = new ProcessBuilder("git", "rev-parse", "HEAD").redirectErrorStream(true).start();
      String out = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
      return p.waitFor() == 0 ? out : "unknown";
    }
    catch (Exception e) {
      return "unknown";
    }
  }

  private void loadKnown(@Nullable String path) throws IOException {
    if (path == null || !Files.exists(Paths.get(path))) return;
    for (String line : Files.readAllLines(Paths.get(path))) {
      if (line.isBlank() || line.startsWith("//")) continue;
      int tab = line.lastIndexOf('\t');
      if (tab > 0) myKnown.put(line.substring(0, tab).trim(), line.substring(tab + 1).trim());
    }
  }

  private void enableInspections() {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    List<String> names = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      // The Dialyzer inspection is only a switch for an external annotator that starts the dialyzer executable.
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
      names.add(ep.implementationClass.substring(ep.implementationClass.lastIndexOf('.') + 1));
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
    JsonObject r = rec("inspections");
    r.add("enabled", GSON.toJsonTree(names));
    journal(r);
    myAcc.inspections = names;
  }

  // =================================================================================================== journal

  private static JsonObject rec(String type) {
    JsonObject o = new JsonObject();
    o.addProperty("t", type);
    return o;
  }

  private synchronized void journal(JsonObject o) {
    if (myKilled) return; // the JVM is going down: what fails now is the shutdown, not the plugin
    try {
      myJournal.write(GSON.toJson(o));
      myJournal.write('\n');
      myJournal.flush();
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private void replay(Path journal) throws IOException {
    JsonObject pendingOp = null;
    for (String line : Files.readAllLines(journal, StandardCharsets.UTF_8)) {
      if (line.isBlank()) continue;
      JsonObject o;
      try {
        o = JsonParser.parseString(line).getAsJsonObject();
      }
      catch (RuntimeException e) {
        continue; // a line cut by a JVM halt
      }
      String t = o.get("t").getAsString();
      if (t.equals("killed")) pendingOp = null; // the run was killed from outside: its last input is run again
      if (t.equals("op")) pendingOp = o;
      if (t.equals("done")) pendingOp = null;
      if (t.equals("input-done")) myDoneInputs.add(o.get("id").getAsString());
      if (t.equals("fail") && "hang".equals(str(o, "detector"))) {
        String id = o.get("id").getAsString();
        myDoneInputs.add(id);
        // A hang in a pass of the scaling probe skips the whole generator: the next pass is the same input or larger.
        java.util.regex.Matcher probe = SCALE_PASS.matcher(id);
        if (probe.matches()) myDoneInputs.add(probe.group(1));
      }
      myAcc.accept(o);
    }
    // An operation that started and never finished killed the JVM (halt of the watchdog, OOM, a crash): skip its input.
    if (pendingOp != null) myDoneInputs.add(pendingOp.get("id").getAsString());
  }

  /** The input id of a pass of the scaling probe, scale/<generator>-<n>.erl#p<pass>. */
  static final java.util.regex.Pattern SCALE_PASS = java.util.regex.Pattern.compile("(scale/.+)-\\d+\\.erl#p\\d+");

  static @Nullable String str(JsonObject o, String key) {
    JsonElement e = o.get(key);
    return e == null || e.isJsonNull() ? null : e.getAsString();
  }

  private Thread startWatchdog() {
    Thread t = new Thread(() -> {
      while (true) {
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
          return;
        }
        long started = myOpStart;
        Thread thread = myOpThread;
        JsonObject ctx = myOpContext;
        if (started == 0 || thread == null || ctx == null) continue;
        if (System.currentTimeMillis() - started < myHangMs) continue;
        Throwable hang = new Throwable("operation did not finish in " + myHangMs + " ms");
        hang.setStackTrace(thread.getStackTrace());
        JsonObject f = failure(ctx.deepCopy(), "hang", hang, null);
        journal(f);
        System.err.println("SWEEP: hang in " + myOpLabel + ", halting the JVM; rerun with SWEEP_RESUME=1");
        Runtime.getRuntime().halt(3);
      }
    }, "sweep-watchdog");
    t.setDaemon(true);
    t.start();
    return t;
  }

  // =================================================================================================== inputs

  static final class Input {
    final String id;
    final String name;
    final byte[] bytes;
    String mutation;
    final Map<String, String> extra = new LinkedHashMap<>();

    Input(String id, String name, byte[] bytes) {
      this.id = id;
      this.name = name;
      this.bytes = bytes;
    }

    Input(String id, String name, String text) {
      this(id, name, text.getBytes(StandardCharsets.UTF_8));
    }

    Input with(String file, String text) {
      extra.put(file, text);
      return this;
    }

    String file() {
      int i = id.lastIndexOf('#');
      return i < 0 ? id : id.substring(0, i);
    }
  }

  private Random rng(String rel, int index, String salt) {
    return new Random(mySeed * 31 + stableHash(rel + "\u0000" + index + "\u0000" + salt));
  }

  /** Token ranges [start, end) of the non-whitespace tokens. */
  private List<int[]> tokens(String text) {
    List<int[]> result = new ArrayList<>();
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    while (lexer.getTokenType() != null) {
      if (lexer.getTokenType() != TokenType.WHITE_SPACE && lexer.getTokenEnd() > lexer.getTokenStart()) {
        result.add(new int[]{lexer.getTokenStart(), lexer.getTokenEnd()});
      }
      lexer.advance();
    }
    return result;
  }

  private static final String[] GARBAGE = {"@#$%^&*", "}])", "([{", "<<<<", "->->", ";;.", "fun", "#{", "?", "\\", "`", "end end",
    "when", ":::", "=>", "||", "\u0000", "😀", "-spec", "#", "$", "~", "\"\"\"", "%", ".", "catch", "maybe", "-define(", "?MODULE:"};
  private static final String[] BRACKETS = {"(", ")", "[", "]", "{", "}", "<<", ">>", "#{", "begin", "end", "case", "of", "fun(", "try", "if"};

  private @Nullable Input mutate(String rel, String name, byte[] bytes, int index) {
    String text = decode(bytes);
    Random r = rng(rel, index, "mutation");
    String kind = myMutationKinds.get(Math.floorMod(index + stableHash(rel), myMutationKinds.size()));
    List<int[]> tokens = tokens(text);
    String mutated;
    String descriptor;
    int len = text.length();
    switch (kind) {
      case "truncate" -> {
        int at = len == 0 ? 0 : r.nextInt(len);
        mutated = text.substring(0, at);
        descriptor = "truncate@" + at;
      }
      case "delete-token" -> {
        if (tokens.isEmpty()) return null;
        int[] t = tokens.get(r.nextInt(tokens.size()));
        mutated = text.substring(0, t[0]) + text.substring(t[1]);
        descriptor = "delete-token@" + t[0] + "-" + t[1];
      }
      case "dup-token" -> {
        if (tokens.isEmpty()) return null;
        int[] t = tokens.get(r.nextInt(tokens.size()));
        mutated = text.substring(0, t[1]) + " " + text.substring(t[0], t[1]) + text.substring(t[1]);
        descriptor = "dup-token@" + t[0] + "-" + t[1];
      }
      case "swap-tokens" -> {
        if (tokens.size() < 2) return null;
        int i = r.nextInt(tokens.size());
        int j = r.nextInt(tokens.size());
        if (i == j) j = (i + 1) % tokens.size();
        int[] a = tokens.get(Math.min(i, j));
        int[] b = tokens.get(Math.max(i, j));
        mutated = text.substring(0, a[0]) + text.substring(b[0], b[1]) + text.substring(a[1], b[0]) + text.substring(a[0], a[1]) +
                  text.substring(b[1]);
        descriptor = "swap-tokens@" + a[0] + "-" + a[1] + "," + b[0] + "-" + b[1];
      }
      case "garbage" -> {
        int at = boundary(r, tokens, len);
        String g = GARBAGE[r.nextInt(GARBAGE.length)];
        mutated = text.substring(0, at) + g + text.substring(at);
        descriptor = "garbage@" + at + ":" + g;
      }
      case "bracket" -> {
        int at = boundary(r, tokens, len);
        String g = BRACKETS[r.nextInt(BRACKETS.length)];
        mutated = text.substring(0, at) + " " + g + " " + text.substring(at);
        descriptor = "bracket@" + at + ":" + g;
      }
      case "unterminated-string" -> {
        int at = boundary(r, tokens, len);
        mutated = text.substring(0, at) + "\"" + text.substring(at);
        descriptor = "unterminated-string@" + at;
      }
      case "unterminated-atom" -> {
        int at = boundary(r, tokens, len);
        mutated = text.substring(0, at) + "'" + text.substring(at);
        descriptor = "unterminated-atom@" + at;
      }
      default -> throw new IllegalStateException(kind);
    }
    Input in = new Input(rel + "#m" + index, name, mutated);
    in.mutation = descriptor;
    return in;
  }

  private static int boundary(Random r, List<int[]> tokens, int len) {
    if (tokens.isEmpty() || r.nextInt(4) == 0) return len == 0 ? 0 : r.nextInt(len + 1);
    int[] t = tokens.get(r.nextInt(tokens.size()));
    return r.nextBoolean() ? t[0] : t[1];
  }

  static String decode(byte[] bytes) {
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.indexOf('�') >= 0) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text;
  }

  // =================================================================================================== running

  private void runInput(Input in, Consumer<JsonObject> sink) {
    myCurrent = in;
    JsonObject start = rec("input");
    start.addProperty("id", in.id);
    start.addProperty("bytes", in.bytes.length);
    if (in.mutation != null) start.addProperty("mutation", in.mutation);
    sink.accept(start);
    String[] original = new String[1];
    boolean ready = runOp("setup", sink, () -> {
      for (Map.Entry<String, String> e : in.extra.entrySet()) {
        myCreated.add(create(e.getKey(), e.getValue().getBytes(StandardCharsets.UTF_8)));
      }
      VirtualFile main = create(in.name, in.bytes);
      myCreated.add(main);
      myFixture.configureFromExistingVirtualFile(main);
      original[0] = myFixture.getEditor().getDocument().getText();
      return 1;
    });
    if (ready) {
      long inputStart = System.currentTimeMillis();
      for (int i = 0; i < myOps.size(); i++) {
        String op = myOps.get(i);
        // An operation cannot be stopped, but the next ones of a slow input can be skipped: one slow input must not take
        // the time budget of the whole run. Its slow operations are findings of their own already.
        if (myInputBudgetMs > 0 && i > 0 && System.currentTimeMillis() - inputStart > myInputBudgetMs) {
          property(sink, "input-capped", in, "the input took more than " + myInputBudgetMs / 1000 + " s; skipped " +
                                             String.join(",", myOps.subList(i, myOps.size())));
          break;
        }
        runOp("restore", sink, () -> {
          restore(original[0]);
          return 0;
        });
        runOp(op, sink, () -> {
          if (!BACKGROUND_OPS.contains(op)) {
            if (myPlant != null) SweepPlants.opFault(op, in.name, getProject(), myFixture.getEditor().getDocument());
            return operation(op, in, original[0], sink);
          }
          String foreground = op.substring(0, op.length() - "-bg".length());
          Document document = myFixture.getEditor().getDocument();
          return inBackground(() -> {
            if (myPlant != null) SweepPlants.opFault(op, in.name, getProject(), document);
            return operation(foreground, in, original[0], sink);
          });
        });
      }
    }
    runOp("cleanup", sink, () -> {
      LookupManager.hideActiveLookup(getProject());
      FileEditorManagerEx.getInstanceEx(getProject()).closeAllFiles();
      WriteAction.run(() -> {
        for (VirtualFile f : myCreated) if (f.isValid()) f.delete(this);
      });
      myCreated.clear();
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      return 0;
    });
    Runtime rt = Runtime.getRuntime();
    myPeakHeapMb = Math.max(myPeakHeapMb, (rt.totalMemory() - rt.freeMemory()) / (1024 * 1024));
    JsonObject done = rec("input-done");
    done.addProperty("id", in.id);
    sink.accept(done);
  }

  private VirtualFile create(String name, byte[] bytes) throws IOException {
    VirtualFile f = myFixture.getTempDirFixture().createFile(name);
    WriteAction.run(() -> f.setBinaryContent(bytes));
    return f;
  }

  private void restore(String original) {
    LookupManager.hideActiveLookup(getProject());
    Document doc = myFixture.getEditor().getDocument();
    if (!doc.getText().equals(original)) {
      WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.setText(original));
    }
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    myFixture.getEditor().getCaretModel().moveToOffset(0);
  }

  private interface Op {
    int run() throws Throwable;
  }

  /** Runs the body as the IDE runs a background computation, while this (dispatch) thread keeps serving its events. */
  private static int inBackground(Op body) throws Throwable {
    // The pooled executor logs what a task throws and returns null, so the task hands its failure over as a value.
    java.util.concurrent.Future<Object> future = ApplicationManager.getApplication().executeOnPooledThread(() -> {
      try {
        return ProgressManager.getInstance().runProcess(() -> ReadAction.compute(() -> {
          try {
            return (Object)body.run();
          }
          catch (Throwable e) {
            return e;
          }
        }), new EmptyProgressIndicator());
      }
      catch (Throwable e) {
        return e;
      }
    });
    while (true) {
      try {
        Object result = future.get(5, java.util.concurrent.TimeUnit.MILLISECONDS);
        if (result instanceof Throwable t) throw t;
        return (Integer)result;
      }
      catch (java.util.concurrent.TimeoutException e) {
        UIUtil.dispatchAllInvocationEvents();
      }
    }
  }

  /** Runs one operation; records exceptions, logged errors and the wall time. Returns false if the operation threw. */
  private boolean runOp(String op, Consumer<JsonObject> sink, Op body) {
    Input in = myCurrent;
    boolean service = op.equals("setup") || op.equals("restore") || op.equals("cleanup");
    JsonObject ctx = new JsonObject();
    ctx.addProperty("id", in.id);
    ctx.addProperty("file", in.file());
    if (in.mutation != null) ctx.addProperty("mutation", in.mutation);
    ctx.addProperty("op", op);
    ctx.addProperty("mode", myMode);
    if (!service && sink == mySink) {
      JsonObject s = rec("op");
      s.addProperty("id", in.id);
      s.addProperty("op", op);
      s.addProperty("ts", System.currentTimeMillis());
      journal(s);
    }
    List<Throwable> logged = Collections.synchronizedList(new ArrayList<>());
    LoggedErrorProcessor processor = new LoggedErrorProcessor() {
      @Override
      public @NotNull Set<Action> processError(@NotNull String category, @NotNull String message, String @NotNull [] details,
                                               @Nullable Throwable t) {
        Throwable e = t != null ? t : new Throwable(message);
        if (t != null && message != null && !message.isEmpty() && !message.equals(t.getMessage())) {
          e = new LoggedError(message, t);
        }
        logged.add(e);
        return EnumSet.noneOf(Action.class);
      }
    };
    myOpThread = Thread.currentThread();
    myOpLabel = in.id + " " + op;
    myOpContext = ctx;
    long t0 = System.currentTimeMillis();
    myOpStart = t0;
    int[] count = new int[1];
    Throwable thrown = null;
    try {
      LoggedErrorProcessor.executeWith(processor, (ThrowableRunnable<Throwable>)() -> {
        try {
          if ("dumb".equals(myMode) && !service) {
            DumbModeTestUtils.runInDumbModeSynchronously(getProject(), () -> count[0] = body.run());
          }
          else {
            count[0] = body.run();
          }
        }
        finally {
          // Let the events queued by the operation run inside its window, so that a late logged error is attributed to it.
          PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        }
      });
    }
    catch (Throwable t) {
      thrown = t;
    }
    long ms = System.currentTimeMillis() - t0;
    myOpStart = 0;
    myOpContext = null;
    if (thrown != null) sink.accept(failure(ctx.deepCopy(), "exception", thrown, in));
    for (Throwable c = thrown; c != null; c = c.getCause()) {
      if (c instanceof OutOfMemoryError && sink == mySink) {
        // The heap is gone and whatever runs next fails for that reason only; the journal has the failure, and a rerun
        // with SWEEP_RESUME=1 skips this input.
        JsonObject d = rec("input-done");
        d.addProperty("id", in.id);
        journal(d);
        System.err.println("SWEEP: OutOfMemoryError in " + in.id + " " + op + ", halting the JVM; rerun with SWEEP_RESUME=1");
        Runtime.getRuntime().halt(4);
      }
    }
    for (Throwable t : new ArrayList<>(logged)) sink.accept(failure(ctx.deepCopy(), "log-error", t, in));
    if (!service) {
      JsonObject d = rec("done");
      d.addProperty("id", in.id);
      d.addProperty("op", op);
      d.addProperty("ms", ms);
      d.addProperty("n", count[0]);
      d.addProperty("bytes", in.bytes.length);
      d.addProperty("failed", thrown != null || !logged.isEmpty());
      sink.accept(d);
    }
    return thrown == null;
  }

  static final class LoggedError extends RuntimeException {
    LoggedError(String message, Throwable cause) {
      super(message, cause);
    }
  }

  private int operation(String op, Input in, String original, Consumer<JsonObject> sink) throws Throwable {
    PsiFile file = myFixture.getFile();
    Document doc = myFixture.getEditor().getDocument();
    PsiDocumentManager pdm = PsiDocumentManager.getInstance(getProject());
    switch (op) {
      case "stubs" -> {
        StubTextInconsistencyException.checkStubTextConsistency(file);
        return 1;
      }
      case "highlight" -> {
        List<HighlightInfo> infos = myFixture.doHighlighting();
        return infos.size();
      }
      case "resolve" -> {
        int n = 0;
        long deadline = System.currentTimeMillis() + myOpBudgetMs;
        for (PsiElement e : SyntaxTraverser.psiTraverser(file)) {
          for (PsiReference ref : e.getReferences()) {
            if (ref instanceof PsiPolyVariantReference poly) poly.multiResolve(false);
            else ref.resolve();
            n++;
          }
          if ((n & 63) == 0 && System.currentTimeMillis() > deadline) {
            property(sink, "truncated", in, "resolve stopped after " + myOpBudgetMs + " ms and " + n + " references");
            break;
          }
        }
        return n;
      }
      case "structure" -> {
        StructureViewBuilder builder = LanguageStructureViewBuilder.getInstance().getStructureViewBuilder(file);
        if (!(builder instanceof TreeBasedStructureViewBuilder tree)) return 0;
        StructureViewModel model = tree.createStructureViewModel(myFixture.getEditor());
        try {
          return walk(model.getRoot(), 0);
        }
        finally {
          Disposer.dispose(model);
        }
      }
      case "folding" -> {
        FoldingBuilder builder = LanguageFolding.INSTANCE.forLanguage(ErlangLanguage.INSTANCE);
        if (builder == null) return 0;
        FoldingDescriptor[] descriptors = LanguageFolding.buildFoldingDescriptors(builder, file, doc, false);
        for (FoldingDescriptor d : descriptors) {
          d.getPlaceholderText();
          ASTNode node = d.getElement();
          builder.isCollapsedByDefault(node);
        }
        return descriptors.length;
      }
      case "completion" -> {
        Random r = rng(in.id, 0, "completion");
        List<int[]> tokens = tokens(original);
        int n = 0;
        for (int i = 0; i < myCompletions; i++) {
          restore(original);
          int offset = Math.min(boundary(r, tokens, original.length()), doc.getTextLength());
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          LookupElement[] items;
          try {
            items = myFixture.completeBasic();
          }
          catch (Throwable t) {
            throw new At("completion at offset " + offset, t);
          }
          n += items == null ? 1 : items.length;
          LookupManager.hideActiveLookup(getProject());
        }
        return n;
      }
      case "usages" -> {
        int n = 0;
        FindUsagesProvider provider = LanguageFindUsages.INSTANCE.forLanguage(ErlangLanguage.INSTANCE);
        for (PsiNamedElement e : picks(file, in, "usages")) {
          if (!provider.canFindUsagesFor(e)) continue;
          try {
            Collection<UsageInfo> usages = myFixture.findUsages(e);
            n += 1 + usages.size();
          }
          catch (Throwable t) {
            throw new At("find usages of " + describe(e), t);
          }
        }
        return n;
      }
      case "rename" -> {
        int n = 0;
        List<PsiNamedElement> picks = picks(file, in, "rename");
        List<int[]> ranges = new ArrayList<>();
        List<String> kinds = new ArrayList<>();
        for (PsiNamedElement e : picks) {
          ranges.add(new int[]{e.getTextRange().getStartOffset(), e.getTextRange().getEndOffset()});
          kinds.add(e.getClass().getName());
        }
        for (int i = 0; i < ranges.size(); i++) {
          restore(original);
          PsiNamedElement e = find(myFixture.getFile(), ranges.get(i), kinds.get(i));
          if (e == null) continue;
          String newName = e instanceof ErlangQVar ? "SweepRenamed" : e instanceof ErlangMacrosDefinition ? "SWEEP_RENAMED" : "sweep_renamed";
          String what = describe(e);
          try {
            myFixture.renameElement(e, newName);
            n++;
          }
          catch (Throwable t) {
            throw new At("rename of " + what + " to " + newName, t);
          }
        }
        return n;
      }
      case "format" -> {
        WriteCommandAction.runWriteCommandAction(getProject(), () -> {
          CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile());
        });
        pdm.commitAllDocuments();
        String once = doc.getText();
        if (!stripWs(once).equals(stripWs(original))) {
          property(sink, "format-changes-code", in, firstDiff(stripWs(original), stripWs(once)));
        }
        WriteCommandAction.runWriteCommandAction(getProject(), () -> {
          CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile());
        });
        pdm.commitAllDocuments();
        String twice = doc.getText();
        if (!twice.equals(once)) property(sink, "idempotence", in, firstDiff(once, twice));
        return once.equals(original) ? 1 : 2;
      }
      case "reparse" -> {
        Random r = rng(in.id, 0, "reparse");
        String[] inserts = {"x", " ", "\n", "(", ")", ",", ".", "\"", "'", "%", "->", "X = 1,", "end", "{", "}", ";", "f() -> ok.\n", "<<", "#", "?"};
        int n = 0;
        for (int i = 0; i < 4; i++) {
          int length = doc.getTextLength();
          int offset = length == 0 ? 0 : r.nextInt(length + 1);
          boolean delete = length > 0 && offset < length && r.nextInt(4) == 0;
          String insert = inserts[r.nextInt(inserts.length)];
          WriteCommandAction.runWriteCommandAction(getProject(), () -> {
            if (delete) doc.deleteString(offset, Math.min(length, offset + 1 + r.nextInt(3)));
            else doc.insertString(offset, insert);
          });
          pdm.commitAllDocuments();
          try {
            if (shallow(myFixture.getFile(), in, sink)) PsiTestUtil.checkPsiMatchesTextIgnoringNonCode(myFixture.getFile());
            StubTextInconsistencyException.checkStubTextConsistency(myFixture.getFile());
          }
          catch (Throwable t) {
            throw new At("edit " + i + ": " + (delete ? "delete at " : "insert '" + insert.replace("\n", "\\n") + "' at ") + offset, t);
          }
          n++;
        }
        return n;
      }
      case "typing" -> {
        Random r = rng(in.id, 0, "typing");
        // Start of a seeded line, so the typed function begins between forms more often than not.
        int lines = doc.getLineCount();
        int offset = lines == 0 ? 0 : doc.getLineStartOffset(r.nextInt(lines));
        myFixture.getEditor().getCaretModel().moveToOffset(offset);
        String typed = "sweep(X, [H|T]) when is_list(T) ->\ncase {X, \"s\", 'a b'} of\n{_, _, _} -> #{k => <<\"b\">>};\n_ -> fun(Y) -> Y end\nend.\n";
        int n = 0;
        for (char c : typed.toCharArray()) {
          try {
            myFixture.type(c);
          }
          catch (Throwable t) {
            throw new At("typing '" + (c == '\n' ? "\\n" : String.valueOf(c)) + "' (char " + n + ") of the session started at offset " + offset, t);
          }
          n++;
        }
        pdm.commitAllDocuments();
        if (shallow(myFixture.getFile(), in, sink)) PsiTestUtil.checkPsiMatchesTextIgnoringNonCode(myFixture.getFile());
        return n;
      }
      default -> throw new IllegalStateException(op);
    }
  }

  /**
   * The PSI/text check dumps the tree with one indent per level, which is quadratic in the depth and ran out of heap on a
   * 20000-deep expression. On such a tree the check is skipped and that is recorded.
   */
  private boolean shallow(PsiFile file, Input in, Consumer<JsonObject> sink) {
    int depth = 0;
    int max = 0;
    PsiElement e = file;
    while (e != null) {
      PsiElement child = e.getFirstChild();
      if (child != null) {
        e = child;
        if (++depth > max) max = depth;
        continue;
      }
      while (e != null && e != file && e.getNextSibling() == null) {
        e = e.getParent();
        depth--;
      }
      if (e == null || e == file) break;
      e = e.getNextSibling();
    }
    if (max <= 1000) return true;
    if (mySkippedDeep.add(in.id)) property(sink, "psi-text-check-skipped", in, "PSI tree depth " + max);
    return false;
  }

  private final Set<String> mySkippedDeep = new HashSet<>();

  /** Carries where inside an operation the failure happened; the signature is taken from the cause. */
  static final class At extends RuntimeException {
    At(String message, Throwable cause) {
      super(message, cause);
    }
  }

  private static int walk(TreeElement element, int depth) {
    element.getPresentation().getPresentableText();
    element.getPresentation().getLocationString();
    int n = 1;
    if (depth > 50) return n;
    for (TreeElement child : element.getChildren()) n += walk(child, depth + 1);
    return n;
  }

  private List<PsiNamedElement> picks(PsiFile file, Input in, String salt) {
    List<List<PsiNamedElement>> byKind = new ArrayList<>();
    for (int i = 0; i < 5; i++) byKind.add(new ArrayList<>());
    for (PsiElement e : SyntaxTraverser.psiTraverser(file)) {
      if (e instanceof ErlangFunction f) byKind.get(0).add(f);
      else if (e instanceof ErlangRecordDefinition d) byKind.get(1).add(d);
      else if (e instanceof ErlangMacrosDefinition d) byKind.get(2).add(d);
      else if (e instanceof ErlangTypeDefinition d) byKind.get(3).add(d);
      else if (e instanceof ErlangQVar v) byKind.get(4).add(v);
    }
    Random r = rng(in.id, 0, salt);
    List<PsiNamedElement> result = new ArrayList<>();
    for (List<PsiNamedElement> list : byKind) {
      if (list.isEmpty()) continue;
      int take = list == byKind.get(4) ? myPicks : Math.max(1, myPicks / 2);
      for (int i = 0; i < take; i++) {
        PsiNamedElement e = list.get(r.nextInt(list.size()));
        if (e.getName() != null && !result.contains(e)) result.add(e);
      }
    }
    return result;
  }

  private static @Nullable PsiNamedElement find(PsiFile file, int[] range, String className) {
    PsiElement e = file.findElementAt(range[0]);
    while (e != null && !(e instanceof PsiFile)) {
      if (e.getClass().getName().equals(className) && e.getTextRange().getStartOffset() == range[0] &&
          e.getTextRange().getEndOffset() == range[1]) {
        return (PsiNamedElement)e;
      }
      e = e.getParent();
    }
    return null;
  }

  private static String describe(PsiNamedElement e) {
    String cls = e.getClass().getSimpleName().replace("Impl", "").replace("Erlang", "");
    return cls + " '" + e.getName() + "' at offset " + e.getTextRange().getStartOffset();
  }

  /** Removes what the Erlang scanner takes for white space: 0..32 and 128..160 (so NUL and NBSP too). */
  private static String stripWs(String s) {
    StringBuilder sb = new StringBuilder(s.length());
    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (c <= ' ' || c >= 128 && c <= 160) continue;
      sb.append(c);
    }
    return sb.toString();
  }

  private static String firstDiff(String a, String b) {
    int i = 0;
    while (i < a.length() && i < b.length() && a.charAt(i) == b.charAt(i)) i++;
    return "first difference at " + i + ": '" + clip(a, i) + "' vs '" + clip(b, i) + "'";
  }

  private static String clip(String s, int at) {
    return s.substring(Math.max(0, at - 20), Math.min(s.length(), at + 40)).replace("\n", "\\n");
  }

  private void property(Consumer<JsonObject> sink, String kind, Input in, String detail) {
    JsonObject p = rec("prop");
    p.addProperty("kind", kind);
    p.addProperty("id", in.id);
    p.addProperty("file", in.file());
    if (in.mutation != null) p.addProperty("mutation", in.mutation);
    p.addProperty("mode", myMode);
    p.addProperty("detail", detail);
    sink.accept(p);
  }

  // =================================================================================================== failures

  private JsonObject failure(JsonObject f, String detector, Throwable t, @Nullable Input in) {
    f.addProperty("t", "fail");
    f.addProperty("detector", detector);
    f.addProperty("seed", mySeed);
    String where = null;
    Throwable cause = t;
    while (cause instanceof At && cause.getCause() != null) {
      where = cause.getMessage();
      cause = cause.getCause();
    }
    if (where != null) f.addProperty("where", where);
    Signature s = signature(cause, detector, str(f, "op"));
    f.addProperty("signature", s.text);
    f.addProperty("known", myKnown.getOrDefault(s.text, "new"));
    f.addProperty("exceptionClass", s.root.getClass().getName());
    String message = String.valueOf(s.root.getMessage());
    int nl = message.indexOf('\n');
    f.addProperty("message", clipLine(nl < 0 ? message : message.substring(0, nl)));
    f.addProperty("frame", s.frame);
    StringWriter sw = new StringWriter();
    cause.printStackTrace(new PrintWriter(sw));
    String[] lines = sw.toString().split("\n");
    f.addProperty("stack", String.join("\n", Arrays.copyOf(lines, Math.min(lines.length, 150))));
    if (in != null && myDumpDir != null && (in.mutation != null || !in.extra.isEmpty() || in.id.startsWith("synthetic/"))) {
      dump(in, s.text, f);
    }
    return f;
  }

  private static String clipLine(String s) {
    return s.length() > 400 ? s.substring(0, 400) + "..." : s;
  }

  private final Map<String, Integer> myDumped = new LinkedHashMap<>();

  private void dump(Input in, String signature, JsonObject f) {
    int n = myDumped.merge(signature, 1, Integer::sum);
    if (n > 2) return;
    try {
      Path dir = myDumpDir.resolve(String.format("%08x-%d", stableHash(signature), n));
      Files.createDirectories(dir);
      Files.write(dir.resolve(in.name), in.bytes);
      for (Map.Entry<String, String> e : in.extra.entrySet()) Files.writeString(dir.resolve(e.getKey()), e.getValue());
      Files.writeString(dir.resolve("failure.txt"), signature + "\n" + in.id + "\n" + in.mutation + "\n");
      f.addProperty("dump", dir.toString());
    }
    catch (IOException ignored) {
    }
  }

  record Signature(String text, Throwable root, @Nullable String frame) {
  }

  static boolean pluginFrame(StackTraceElement e) {
    String c = e.getClassName();
    return c.startsWith("org.intellij.erlang.") && !c.startsWith(HARNESS_CLASS) && !c.startsWith(BASE_CLASS);
  }

  static Signature signature(Throwable t, String detector, @Nullable String op) {
    List<Throwable> chain = new ArrayList<>();
    for (Throwable c = t; c != null && chain.size() < 20; c = c.getCause()) chain.add(c);
    Throwable root = chain.get(chain.size() - 1);
    if (root instanceof LoggedError && chain.size() > 1) root = chain.get(chain.size() - 2);
    StackTraceElement frame = null;
    for (int i = chain.size() - 1; i >= 0 && frame == null; i--) {
      for (StackTraceElement e : chain.get(i).getStackTrace()) {
        if (pluginFrame(e)) {
          frame = e;
          break;
        }
      }
    }
    String kind = null;
    for (Throwable c : chain) {
      String cls = c.getClass().getName();
      String msg = String.valueOf(c.getMessage());
      if (c instanceof StackOverflowError) kind = "StackOverflow";
      else if (c instanceof OutOfMemoryError) kind = "OutOfMemory";
      else if (cls.endsWith("IndexNotReadyException")) kind = "IndexNotReady";
      else if (cls.endsWith("StubTextInconsistencyException")) kind = "StubTextMismatch";
      else if (msg.contains("Read access is allowed")) kind = "ReadAccess";
      else if (msg.contains("Event Dispatch Thread") && msg.contains("only")) kind = "EdtOnly";
      else if (msg.contains("Write access is allowed")) kind = "WriteAccess";
      else if (msg.contains("Slow operations are prohibited")) kind = "SlowOps";
      else if (msg.contains("Access to tree elements not allowed")) kind = "TreeAccess";
      if (kind != null) break;
    }
    if (kind == null && frame == null) {
      // The PSI/text check of the test framework fails with a plain assertion: name it after what it compares.
      for (StackTraceElement e : root.getStackTrace()) {
        if (!e.getClassName().equals("com.intellij.testFramework.PsiTestUtil")) continue;
        if (e.getMethodName().startsWith("checkPsiMatchesText") || e.getMethodName().equals("compareFromAllRoots")) kind = "PsiTextMismatch";
        if (kind != null) break;
      }
    }
    for (Throwable c : chain) {
      if (c instanceof StubTextInconsistencyException stubs) {
        // No plugin frame can tell two causes apart here, so the first stub that differs is part of the key.
        String text = "StubTextMismatch @ <no plugin frame> " + firstStubDifference(stubs.getStubsFromText(), stubs.getStubsFromPsi());
        return new Signature(text, c, null);
      }
    }
    if (kind == null && detector.equals("hang")) kind = "Hang";
    if (kind == null) kind = root.getClass().getSimpleName();
    String where;
    if (frame != null) {
      where = frame.getClassName() + "." + frame.getMethodName().replaceAll("^lambda\\$(.+)\\$\\d+$", "$1");
    }
    else {
      StackTraceElement[] st = root.getStackTrace();
      String top = "?";
      for (StackTraceElement e : st) {
        if (!e.getClassName().startsWith("java.") && !e.getClassName().startsWith("jdk.") &&
            !e.getClassName().startsWith("com.intellij.openapi.diagnostic.") && !e.getClassName().startsWith("org.junit.") &&
            !e.getClassName().startsWith("junit.") && !e.getClassName().startsWith("com.intellij.testFramework.TestLogger")) {
          top = e.getClassName() + "." + e.getMethodName();
          break;
        }
      }
      // Without a plugin frame the operation and the first platform frame are what tells two failures apart.
      where = "<no plugin frame> " + top + (op == null ? "" : " [" + op.replace("-bg", "") + "]");
    }
    String frameText = frame == null ? null : frame.getClassName() + "." + frame.getMethodName() + "(" + frame.getFileName() + ":" +
                                              frame.getLineNumber() + ")";
    return new Signature(kind + " @ " + where, root, frameText);
  }

  /** "ERL_FUNCTION missing in PSI", "ERL_FUNCTION extra in PSI" or "ERL_X in text, ERL_Y in PSI". */
  static String firstStubDifference(String fromText, String fromPsi) {
    String[] text = fromText.split("\n");
    String[] psi = fromPsi.split("\n");
    int i = 0;
    while (i < text.length && i < psi.length && text[i].trim().equals(psi[i].trim())) i++;
    String t = i < text.length ? stubType(text[i]) : null;
    String p = i < psi.length ? stubType(psi[i]) : null;
    if (t == null && p == null) return "no difference in the stub trees";
    if (p == null) return t + " missing in PSI";
    if (t == null) return p + " extra in PSI";
    boolean psiSkipsOne = i + 1 < text.length && text[i + 1].trim().equals(psi[i].trim());
    boolean textSkipsOne = i + 1 < psi.length && psi[i + 1].trim().equals(text[i].trim());
    if (psiSkipsOne && !textSkipsOne) return t + " missing in PSI";
    if (textSkipsOne && !psiSkipsOne) return p + " extra in PSI";
    return t.equals(p) ? t + " differs" : t + " in text, " + p + " in PSI";
  }

  private static String stubType(String line) {
    String s = line.trim();
    int colon = s.indexOf(':');
    return colon > 0 ? s.substring(0, colon) : s;
  }

  // =================================================================================================== accumulator

  /** Builds the totals from journal records, so a resumed run and a fresh one report the same way. */
  static final class Acc {
    List<String> inspections = List.of();
    int inputs;
    int originals;
    int mutated;
    int other;
    final Map<String, int[]> ops = new TreeMap<>(); // op -> {runs, failedRuns, activity, totalMs}
    final Map<String, long[]> opMs = new TreeMap<>();
    final List<JsonObject> failures = new ArrayList<>();
    final Map<String, List<JsonObject>> bySignature = new LinkedHashMap<>();
    final List<JsonObject> props = new ArrayList<>();
    final List<JsonObject> slowest = new ArrayList<>();
    final List<JsonObject> scaling = new ArrayList<>();
    final List<JsonObject> controls = new ArrayList<>();
    final Map<String, Integer> mutationKinds = new TreeMap<>();
    long slowMs = 10000;

    void accept(JsonObject o) {
      switch (o.get("t").getAsString()) {
        case "run" -> slowMs = o.getAsJsonObject("config").get("slowMs").getAsLong();
        case "inspections" -> {
          List<String> names = new ArrayList<>();
          for (JsonElement e : o.getAsJsonArray("enabled")) names.add(e.getAsString());
          inspections = names;
        }
        case "input" -> {
          inputs++;
          String id = o.get("id").getAsString();
          if (id.endsWith("#orig")) originals++;
          else if (o.has("mutation")) {
            mutated++;
            String m = o.get("mutation").getAsString();
            mutationKinds.merge(m.substring(0, m.indexOf('@')), 1, Integer::sum);
          }
          else other++;
        }
        case "done" -> {
          String op = o.get("op").getAsString();
          int[] c = ops.computeIfAbsent(op, k -> new int[3]);
          c[0]++;
          if (o.get("failed").getAsBoolean()) c[1]++;
          c[2] += o.get("n").getAsInt();
          opMs.computeIfAbsent(op, k -> new long[1])[0] += o.get("ms").getAsLong();
          slowest.add(o);
          if (slowest.size() > 400) trimSlowest();
        }
        case "fail" -> {
          failures.add(o);
          bySignature.computeIfAbsent(o.get("signature").getAsString(), k -> new ArrayList<>()).add(o);
        }
        case "prop" -> props.add(o);
        case "scale" -> scaling.add(o);
        case "control" -> controls.add(o);
        default -> {
        }
      }
    }

    private void trimSlowest() {
      slowest.sort((a, b) -> Long.compare(b.get("ms").getAsLong(), a.get("ms").getAsLong()));
      // keep everything over the threshold, and the top 30 otherwise
      int keep = 30;
      while (keep < slowest.size() && slowest.get(keep).get("ms").getAsLong() > slowMs) keep++;
      while (slowest.size() > keep) slowest.remove(slowest.size() - 1);
    }

    JsonObject totals(int files, long wall, boolean complete, Map<String, String> known) {
      JsonObject totals = new JsonObject();
      totals.addProperty("complete", complete);
      totals.addProperty("wallMs", wall);
      totals.addProperty("files", files);
      totals.addProperty("inputs", inputs);
      totals.addProperty("originalInputs", originals);
      totals.addProperty("mutatedInputs", mutated);
      totals.addProperty("otherInputs", other);
      totals.add("mutationKinds", GSON.toJsonTree(mutationKinds));
      JsonObject opsJson = new JsonObject();
      for (Map.Entry<String, int[]> e : ops.entrySet()) {
        JsonObject j = new JsonObject();
        j.addProperty("runs", e.getValue()[0]);
        j.addProperty("failedRuns", e.getValue()[1]);
        j.addProperty("activity", e.getValue()[2]);
        j.addProperty("totalMs", opMs.get(e.getKey())[0]);
        opsJson.add(e.getKey(), j);
      }
      totals.add("ops", opsJson);
      totals.addProperty("failures", failures.size());
      totals.addProperty("signatures", bySignature.size());
      int fresh = 0;
      for (String s : bySignature.keySet()) if (!known.containsKey(s)) fresh++;
      totals.addProperty("newSignatures", fresh);
      Map<String, Integer> propCounts = new TreeMap<>();
      for (JsonObject p : props) propCounts.merge(p.get("kind").getAsString(), 1, Integer::sum);
      totals.add("properties", GSON.toJsonTree(propCounts));
      int slow = 0;
      for (JsonObject s : slowest) if (s.get("ms").getAsLong() > slowMs) slow++;
      totals.addProperty("slowOperations", slow);
      return totals;
    }

    JsonObject report(JsonObject config, int files, long wall, boolean complete, Map<String, String> known) {
      trimSlowest();
      JsonObject r = new JsonObject();
      r.add("config", config);
      r.add("inspections", GSON.toJsonTree(inspections));
      r.add("totals", totals(files, wall, complete, known));
      r.add("controls", GSON.toJsonTree(controls));
      JsonArray sigs = new JsonArray();
      JsonArray fails = new JsonArray();
      for (Map.Entry<String, List<JsonObject>> e : bySignature.entrySet()) {
        List<JsonObject> list = e.getValue();
        JsonObject s = new JsonObject();
        s.addProperty("signature", e.getKey());
        s.addProperty("known", known.getOrDefault(e.getKey(), "new"));
        s.addProperty("count", list.size());
        Set<String> opsSeen = new LinkedHashSet<>();
        Set<String> detectors = new LinkedHashSet<>();
        Set<String> inputsSeen = new LinkedHashSet<>();
        for (JsonObject f : list) {
          opsSeen.add(str(f, "op"));
          detectors.add(str(f, "detector"));
          inputsSeen.add(str(f, "id"));
        }
        s.add("operations", GSON.toJsonTree(opsSeen));
        s.add("detectors", GSON.toJsonTree(detectors));
        s.addProperty("inputs", inputsSeen.size());
        JsonObject first = list.get(0);
        for (String k : List.of("exceptionClass", "message", "frame")) s.add(k, first.get(k));
        s.addProperty("firstInput", str(first, "id"));
        sigs.add(s);
        // Full stacks for the first 3 failures of a signature; the others keep everything but the stack.
        for (int i = 0; i < list.size() && i < 200; i++) {
          JsonObject f = list.get(i).deepCopy();
          f.remove("t");
          if (i >= 3) f.remove("stack");
          fails.add(f);
        }
      }
      r.add("signatures", sigs);
      r.add("failures", fails);
      JsonArray propsJson = new JsonArray();
      Map<String, Integer> perKind = new TreeMap<>();
      for (JsonObject p : props) {
        if (perKind.merge(p.get("kind").getAsString(), 1, Integer::sum) > 100) continue;
        JsonObject c = p.deepCopy();
        c.remove("t");
        propsJson.add(c);
      }
      r.add("properties", propsJson);
      JsonArray slow = new JsonArray();
      for (JsonObject s : slowest) {
        JsonObject c = s.deepCopy();
        c.remove("t");
        c.remove("failed");
        slow.add(c);
      }
      r.add("slowest", slow);
      r.add("scaling", GSON.toJsonTree(scaling));
      return r;
    }

    JsonObject ledger(JsonObject config, int files, long wall, boolean complete, Map<String, String> known) {
      JsonObject l = new JsonObject();
      for (String k : List.of("date", "revision", "mode", "seed", "corpusId", "shard", "limit", "originals", "mutations",
                              "mutationKinds", "ops", "synthetic", "scale", "plant", "only", "budgetMinutes", "corpusSet", "sdkRelease")) {
        l.add(k, config.get(k));
      }
      l.add("totals", totals(files, wall, complete, known));
      JsonObject sigs = new JsonObject();
      for (Map.Entry<String, List<JsonObject>> e : bySignature.entrySet()) sigs.addProperty(e.getKey(), e.getValue().size());
      l.add("signatures", sigs);
      return l;
    }
  }

  // =================================================================================================== scaling probe

  /** Returns false when it stopped on the time budget of the run. */
  private boolean scalingProbe(long start) {
    for (Synthetic.Scalable s : Synthetic.scalables()) {
      if (skip("scale/" + s.name)) continue;
      if (budgetExceeded(start)) return false;
      Map<Integer, Map<String, Long>> times = new LinkedHashMap<>();
      // The first run at the base size warms up the JIT and the caches and is not measured.
      for (int pass = 0; pass < 3; pass++) {
        int n = pass == 2 ? s.n * 4 : s.n;
        Map<String, Long> ms = new LinkedHashMap<>();
        boolean[] capped = new boolean[1];
        Consumer<JsonObject> sink = rec -> {
          String t = rec.get("t").getAsString();
          if (t.equals("done")) ms.put(rec.get("op").getAsString(), rec.get("ms").getAsLong());
          if (t.equals("prop") && "input-capped".equals(str(rec, "kind"))) capped[0] = true;
          if (t.equals("fail") || t.equals("prop")) {
            journal(rec);
            myAcc.accept(rec);
          }
        };
        runInput(new Input("scale/" + s.name + "-" + n + ".erl#p" + pass, "scale_" + s.name + ".erl", s.text.apply(n)), sink);
        if (pass > 0) times.put(n, ms);
        // The base size already took the time of a whole input: the larger one would take four times that at least.
        if (capped[0]) break;
      }
      for (String op : myOps) {
        Long base = times.getOrDefault(s.n, Map.of()).get(op);
        Long big = times.getOrDefault(s.n * 4, Map.of()).get(op);
        if (base == null || big == null) continue;
        JsonObject r = rec("scale");
        r.addProperty("input", s.name);
        r.addProperty("op", op);
        r.addProperty("n", s.n);
        r.addProperty("ms", base);
        r.addProperty("n4", s.n * 4);
        r.addProperty("ms4", big);
        double ratio = base >= 20 ? Math.round(big * 100.0 / base) / 100.0 : 0;
        if (base >= 20) r.addProperty("ratio", ratio);
        else r.addProperty("note", "base run too fast to give a ratio");
        mySink.accept(r);
        if (ratio > myScaleRatio) {
          // Not an exception, but a finding with a key of its own: the probe and the operation.
          JsonObject f = rec("fail");
          f.addProperty("id", "scale/" + s.name);
          f.addProperty("file", "scale/" + s.name);
          f.addProperty("op", op);
          f.addProperty("mode", myMode);
          f.addProperty("detector", "scaling");
          f.addProperty("seed", mySeed);
          String signature = "Scaling @ " + s.name + " " + op.replace("-bg", "");
          f.addProperty("signature", signature);
          f.addProperty("known", myKnown.getOrDefault(signature, "new"));
          f.addProperty("exceptionClass", "none");
          f.addProperty("message", "input 4 times longer (n=" + s.n + " to " + s.n * 4 + ") costs " + ratio + " times more: " + base + " ms to " + big + " ms");
          mySink.accept(f);
        }
      }
      // A resumed run skips the generators that are done.
      JsonObject probed = rec("input-done");
      probed.addProperty("id", "scale/" + s.name);
      journal(probed);
    }
    return true;
  }

  // =================================================================================================== minimiser

  /**
   * Delta debugging over lines, then over tokens: keeps a reduction if any failure of the reduced input has a signature
   * that contains the given text. Writes the result next to the report (SWEEP_MIN_OUT, default build/sweep/min).
   */
  private void minimize(Path root, String target) throws IOException {
    Path out = Paths.get(env("SWEEP_MIN_OUT", "build/sweep/min")).toAbsolutePath();
    Files.createDirectories(out);
    for (Path file : selectFiles(root)) {
      String name = file.getFileName().toString();
      String text = decode(Files.readAllBytes(file));
      if (!triggers(name, text, target)) {
        System.out.println("SWEEP: minimise " + name + ": the input does not trigger '" + target + "'");
        continue;
      }
      List<String> units = new ArrayList<>();
      for (String line : text.split("(?<=\n)")) units.add(line);
      units = ddmin(name, units, target);
      List<String> tokens = new ArrayList<>();
      String joined = String.join("", units);
      int last = 0;
      for (int[] t : tokens(joined)) {
        tokens.add(joined.substring(last, t[1]));
        last = t[1];
      }
      if (last < joined.length()) tokens.add(joined.substring(last));
      tokens = ddmin(name, tokens, target);
      String result = String.join("", tokens);
      Files.writeString(out.resolve(name), result);
      System.out.println("SWEEP: minimised " + name + " from " + text.length() + " to " + result.length() + " chars");
    }
  }

  private boolean triggers(String name, String text, String target) {
    boolean[] hit = new boolean[1];
    runInput(new Input("min/" + name, name, text), rec -> {
      if (rec.get("t").getAsString().equals("fail") && rec.get("signature").getAsString().contains(target)) hit[0] = true;
    });
    return hit[0];
  }

  private List<String> ddmin(String name, List<String> units, String target) {
    int n = 2;
    while (units.size() >= 2) {
      int chunk = Math.max(1, units.size() / n);
      boolean reduced = false;
      for (int start = 0; start < units.size(); start += chunk) {
        List<String> candidate = new ArrayList<>(units.subList(0, start));
        candidate.addAll(units.subList(Math.min(units.size(), start + chunk), units.size()));
        if (triggers(name, String.join("", candidate), target)) {
          units = candidate;
          n = Math.max(n - 1, 2);
          reduced = true;
          break;
        }
      }
      if (!reduced) {
        if (chunk == 1) break;
        n = Math.min(units.size(), n * 2);
      }
    }
    return units;
  }

  // =================================================================================================== synthetic inputs

  static final class Synthetic {
    record Scalable(String name, int n, java.util.function.IntFunction<String> text) {
    }

    private static String module(String name, String body) {
      return "-module(" + name + ").\n-export([f/0]).\n" + body;
    }

    static String chain(int n) {
      return module("scale_chain", "f() -> 1" + " + 1".repeat(n) + ".\n");
    }

    static String nestedTuples(int n) {
      return module("scale_tuples", "f() -> " + "{".repeat(n) + "1" + "}".repeat(n) + ".\n");
    }

    static String nestedLists(int n) {
      return module("scale_lists", "f() -> " + "[".repeat(n) + "1" + "]".repeat(n) + ".\n");
    }

    static String nestedFuns(int n) {
      return module("scale_funs", "f() -> " + "fun() -> ".repeat(n) + "ok" + " end".repeat(n) + ".\n");
    }

    static String nestedCases(int n) {
      return module("scale_cases", "f() -> X = 1, " + "case X of _ -> ".repeat(n) + "ok" + " end".repeat(n) + ".\n");
    }

    static String repeat(int n) {
      StringBuilder sb = new StringBuilder("-module(scale_repeat).\n-export([f/0]).\nf() -> ok.\n");
      for (int i = 0; i < n; i++) sb.append("g").append(i).append("(X) -> {X, g").append(i == 0 ? 0 : i - 1).append("(X)}.\n");
      return sb.toString();
    }

    static String sameName(int n) {
      return "-module(scale_same).\n-export([f/0]).\n" + "f() -> ok.\n".repeat(n);
    }

    static String clauses(int n) {
      StringBuilder sb = new StringBuilder("-module(scale_clauses).\n-export([f/1]).\n");
      for (int i = 0; i < n; i++) sb.append("f(").append(i).append(") -> ").append(i).append(";\n");
      return sb.append("f(_) -> ok.\n").toString();
    }

    static String variables(int n) {
      StringBuilder sb = new StringBuilder("-module(scale_vars).\n-export([f/0]).\nf() ->\n");
      for (int i = 0; i < n; i++) sb.append("  V").append(i).append(" = ").append(i == 0 ? "0" : "V" + (i - 1)).append(",\n");
      return sb.append("  ok.\n").toString();
    }

    static String records(int n) {
      StringBuilder sb = new StringBuilder("-module(scale_records).\n-export([f/0]).\n");
      for (int i = 0; i < n; i++) sb.append("-record(r").append(i).append(", {a, b = #r").append(i == 0 ? 0 : i - 1).append("{}}).\n");
      return sb.append("f() -> #r0{}.\n").toString();
    }

    static String macros(int n) {
      StringBuilder sb = new StringBuilder("-module(scale_macros).\n-export([f/0]).\n");
      for (int i = 0; i < n; i++) sb.append("-define(M").append(i).append(", ").append(i == 0 ? "0" : "?M" + (i - 1)).append(").\n");
      return sb.append("f() -> ?M").append(n - 1).append(".\n").toString();
    }

    static List<Scalable> scalables() {
      return List.of(new Scalable("chain", 500, Synthetic::chain),
                     new Scalable("nested-tuples", 60, Synthetic::nestedTuples),
                     new Scalable("nested-lists", 60, Synthetic::nestedLists),
                     new Scalable("nested-funs", 40, Synthetic::nestedFuns),
                     new Scalable("nested-cases", 40, Synthetic::nestedCases),
                     new Scalable("repeat", 500, Synthetic::repeat),
                     new Scalable("same-name", 500, Synthetic::sameName),
                     new Scalable("clauses", 500, Synthetic::clauses),
                     new Scalable("variables", 500, Synthetic::variables),
                     new Scalable("records", 300, Synthetic::records),
                     new Scalable("macros", 300, Synthetic::macros));
    }

    private static Input in(String name, String file, String text) {
      return new Input("synthetic/" + name, file, text);
    }

    private static Input in(String name, String file, byte[] bytes) {
      return new Input("synthetic/" + name, file, bytes);
    }

    /** The fixed inputs, then the generated ones ({@link SweepGenerated}, partly seeded), then the large ones. */
    static List<Input> inputs(long seed, java.util.function.Function<String, List<int[]>> tokens) {
      List<Input> r = new ArrayList<>();
      r.add(in("empty", "empty.erl", ""));
      r.add(in("empty-hrl", "empty.hrl", ""));
      r.add(in("whitespace", "ws.erl", " \n\t\n"));
      r.add(in("bom-only", "bom.erl", new byte[]{(byte)0xEF, (byte)0xBB, (byte)0xBF}));
      r.add(in("bom-module", "bom_module.erl", concat(new byte[]{(byte)0xEF, (byte)0xBB, (byte)0xBF},
                                                     "-module(bom_module).\n-export([f/0]).\nf() -> ok.\n".getBytes(StandardCharsets.UTF_8))));
      r.add(in("crlf", "crlf.erl", "-module(crlf).\r\n-export([f/0]).\r\nf() ->\r\n  \"a\r\nb\",\r\n  ok.\r\n"));
      r.add(in("cr-only", "cr.erl", "-module(cr).\r-export([f/0]).\rf() -> ok.\r"));
      r.add(in("non-utf8", "latin.erl", concat("-module(latin).\n-export([f/0]).\n%% caf".getBytes(StandardCharsets.UTF_8),
                                               new byte[]{(byte)0xE9, '\n', 'f', '(', ')', ' ', '-', '>', ' ', '"', (byte)0xFC, (byte)0xFF, (byte)0xC0, '"', '.', '\n'})));
      r.add(in("nul-bytes", "nul.erl", new byte[]{'-', 'm', 'o', 'd', 'u', 'l', 'e', '(', 'n', 'u', 'l', ')', '.', 0, 0, '\n', 'f', '(', ')', ' ', '-', '>', 0, 'o', 'k', '.'}));
      r.add(in("module-mismatch", "actual_name.erl", "-module(other_name).\n-export([f/0]).\nf() -> other_name:f().\n"));
      r.add(in("two-modules", "two.erl", "-module(two).\n-module(three).\n-export([f/0]).\nf() -> ?MODULE.\n"));
      r.add(in("no-module", "nomodule.erl", "f() -> ok.\n"));
      r.add(in("only-comment", "comment.erl", "%% nothing here"));
      r.add(in("only-dot", "dot.erl", "."));
      r.add(in("only-attribute-start", "attr.erl", "-"));
      r.add(in("cyclic-include", "cyc.erl", "-module(cyc).\n-include(\"cyc_a.hrl\").\n-export([f/0]).\nf() -> {?A, ?B, #ra{}, #rb{}}.\n")
              .with("cyc_a.hrl", "-include(\"cyc_b.hrl\").\n-define(A, ?B).\n-record(ra, {x = #rb{}}).\n")
              .with("cyc_b.hrl", "-include(\"cyc_a.hrl\").\n-define(B, ?A).\n-record(rb, {x = #ra{}}).\n"));
      r.add(in("self-include", "selfinc.erl", "-module(selfinc).\n-include(\"selfinc.erl\").\n-export([f/0]).\nf() -> ?X.\n"));
      r.add(in("self-include-hrl", "selfinc.hrl", "-include(\"selfinc.hrl\").\n-include_lib(\"selfinc.hrl\").\n-define(X, ?X).\n-record(r, {a = ?X}).\n"));
      r.add(in("cyclic-behaviour", "beh_a.erl", "-module(beh_a).\n-behaviour(beh_b).\n-behaviour(beh_a).\n-callback init(term()) -> ok.\n-export([init/1, start/0]).\ninit(_) -> ok.\nstart() -> ok.\n")
              .with("beh_b.erl", "-module(beh_b).\n-behaviour(beh_a).\n-callback start() -> ok.\n-export([init/1, start/0]).\ninit(_) -> ok.\nstart() -> ok.\n"));
      r.add(in("cyclic-records", "cycrec.erl", "-module(cycrec).\n-export([f/1]).\n-record(a, {b = #b{}}).\n-record(b, {a = #a{}}).\n-record(c, {c = #c{}}).\nf(#a{b = #b{a = A}}) -> A#a.b#b.a#a.b;\nf(#c{c = C}) -> C#c.c.\n"));
      r.add(in("cyclic-types", "cyctype.erl", "-module(cyctype).\n-export([f/1]).\n-type a() :: b().\n-type b() :: a().\n-type c() :: c().\n-opaque d() :: {d(), a()}.\n-export_type([a/0, d/0]).\n-spec f(a()) -> c().\nf(X) -> X.\n"));
      r.add(in("cyclic-macros", "cycmac.erl", "-module(cycmac).\n-export([f/0]).\n-define(A, ?B).\n-define(B, ?A).\n-define(C(X), ?C(X)).\nf() -> {?A, ?C(1), ?C}.\n"));
      r.add(in("cyclic-import", "cycimp.erl", "-module(cycimp).\n-import(cycimp, [f/0]).\n-import(cycimp, [f/0]).\n-export([f/0, f/0]).\nf() -> f().\n"));
      r.addAll(SweepGenerated.inputs(seed, tokens));
      // Char-by-char typing session: every prefix of a small module is an input of its own.
      String session = "-module(typed).\n-export([f/1]).\n-record(r, {a = 1 :: integer()}).\n-define(M(X), {X}).\n" +
                       "-spec f(list()) -> map().\nf(X) when is_list(X) ->\n  case X of\n    [] -> #{a => \"s\"};\n" +
                       "    [H|T] -> {H, T, <<\"b\">>, fun(Y) -> ?M(Y) end, #r{a = 2}}\n  end.\n";
      for (int i = 1; i <= session.length(); i++) {
        Input p = in("typing-session-" + i, "typed.erl", session.substring(0, i));
        p.mutation = "prefix@" + i;
        r.add(p);
      }
      // The large stress inputs last: they are the slowest, and their findings are known.
      r.add(in("long-chain", "scale_chain.erl", chain(20000)));
      r.add(in("deep-tuples", "scale_tuples.erl", nestedTuples(2000)));
      r.add(in("deep-lists", "scale_lists.erl", nestedLists(2000)));
      r.add(in("deep-funs", "scale_funs.erl", nestedFuns(1000)));
      r.add(in("deep-cases", "scale_cases.erl", nestedCases(1000)));
      r.add(in("deep-parens", "parens.erl", module("parens", "f() -> " + "(".repeat(3000) + "1" + ")".repeat(3000) + ".\n")));
      r.add(in("deep-unclosed", "unclosed.erl", module("unclosed", "f() -> " + "{[(".repeat(1500))));
      r.add(in("long-line", "longline.erl", module("longline", "f() -> \"" + "x".repeat(2_000_000) + "\".\n")));
      r.add(in("long-atom", "longatom.erl", module("longatom", "f() -> '" + "a".repeat(100_000) + "'.\n")));
      r.add(in("long-list", "longlist.erl", module("longlist", "f() -> [" + "1, ".repeat(100_000) + "1].\n")));
      r.add(in("many-clauses", "scale_clauses.erl", clauses(20000)));
      r.add(in("many-variables", "scale_vars.erl", variables(5000)));
      r.add(in("many-records", "scale_records.erl", records(3000)));
      r.add(in("many-macros", "scale_macros.erl", macros(3000)));
      r.add(in("same-name-20k", "scale_same.erl", sameName(20000)));
      r.add(in("huge-repeat-20k", "scale_repeat.erl", repeat(20000)));
      return r;
    }

    private static byte[] concat(byte[] a, byte[] b) {
      byte[] r = Arrays.copyOf(a, a.length + b.length);
      System.arraycopy(b, 0, r, a.length, b.length);
      return r;
    }
  }
}

/**
 * Positive controls (SWEEP_PLANT): faults planted through the platform's extension points, so that each detector and each
 * operation path of the harness is seen reporting a failure. This class is deliberately outside the runner class: its
 * frames count as "plugin frames" for the signature.
 */
final class SweepPlants {
  private static final Logger LOG = Logger.getInstance(SweepPlants.class);

  private SweepPlants() {
  }

  static void install(com.intellij.testFramework.fixtures.CodeInsightTestFixture fixture, com.intellij.openapi.Disposable disposable) {
    fixture.enableInspections(new PlantInspection());
    LanguageFolding.INSTANCE.addExplicitExtension(ErlangLanguage.INSTANCE, new PlantFolding(), disposable);
    com.intellij.openapi.extensions.PluginDescriptor plugin =
      com.intellij.ide.plugins.PluginManagerCore.getPlugin(com.intellij.openapi.extensions.PluginId.getId("org.jetbrains.erlang"));
    com.intellij.psi.impl.source.codeStyle.PostFormatProcessor.EP_NAME.getPoint().registerExtension(new PlantPostFormat(), disposable);
    CompletionContributor.EP.getPoint().registerExtension(
      new com.intellij.codeInsight.completion.CompletionContributorEP("Erlang", PlantCompletion.class.getName(), plugin), disposable);
  }

  /** Throws IndexNotReadyException in dumb mode. */
  static void stubQuery(com.intellij.openapi.project.Project project) {
    com.intellij.psi.stubs.StubIndex.getElements(org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, "plant_index", project,
                                                 GlobalSearchScope.allScope(project), org.intellij.erlang.psi.ErlangNamedElement.class);
  }

  private static String module(String name) {
    return "-module(" + name + ").\n-export([" + name + "/1]).\n" + name + "(X) ->\n  Y = X,\n  Y.\n";
  }

  static List<ErlangRobustnessSweepTest.Input> inputs() {
    List<ErlangRobustnessSweepTest.Input> r = new ArrayList<>();
    for (String name : List.of("plant_clean", "plant_throw", "plant_log", "plant_sleep", "plant_index", "plant_folding",
                               "plant_completion", "plant_format", "plant_op", "plant_edt_log", "plant_pooled_log", "plant_bg_edt", "plant_bg_write",
                               "plant_bg_noread", "plant_slowops", "plant_index_insp", "plant_index_fold", "plant_index_compl")) {
      r.add(new ErlangRobustnessSweepTest.Input("plant/" + name + ".erl", name + ".erl", module(name)));
    }
    // Not a plant but a known product fault (the parser takes a fast path when it builds stubs for the index, and on this
    // text the two trees differ). It shows the stub/text detector firing; the control is not required, because it goes
    // away when the fault is fixed.
    r.add(new ErlangRobustnessSweepTest.Input("plant/natural_stub.erl", "natural_stub.erl", "-module(natural_stub).\n-record(r(\nf() -> ok.\n"));
    return r;
  }

  /** Called by the runner at the start of every operation when SWEEP_PLANT is set. */
  static void opFault(String op, String fileName, com.intellij.openapi.project.Project project, Document document) throws Exception {
    boolean background = op.endsWith("-bg");
    switch (fileName) {
      // Background variants only: what the platform forbids on a pooled thread in a read action.
      case "plant_bg_edt.erl" -> {
        if (background) com.intellij.util.concurrency.ThreadingAssertions.assertEventDispatchThread();
      }
      case "plant_bg_write.erl" -> {
        if (background) document.insertString(0, "%% planted\n");
      }
      case "plant_bg_noread.erl" -> {
        // A thread of its own without a read action, as plugin code that starts one would have.
        if (background) {
          try {
            com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(
              () -> com.intellij.psi.stubs.StubIndex.getElements(org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, "plant", project,
                                                                 GlobalSearchScope.allScope(project), org.intellij.erlang.psi.ErlangNamedElement.class)).get();
          }
          catch (java.util.concurrent.ExecutionException e) {
            if (e.getCause() instanceof RuntimeException r) throw r;
            throw e;
          }
        }
      }
      case "plant_slowops.erl" -> {
        if (!background) com.intellij.util.SlowOperations.assertSlowOperationsAreAllowed();
      }
      case "plant_op.erl" -> throw new IllegalStateException("sweep-plant: operation " + op + " throws");
      case "plant_edt_log.erl" -> LOG.error("sweep-plant: LOG.error on the EDT in " + op);
      case "plant_pooled_log.erl" ->
        com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(
          () -> LOG.error("sweep-plant: LOG.error on a pooled thread in " + op)).get();
      // Throws IndexNotReadyException in dumb mode only.
      case "plant_index.erl" -> {
        if (com.intellij.openapi.project.DumbService.isDumb(project)) {
          com.intellij.psi.stubs.StubIndex.getElements(org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, "plant_index", project,
                                                       GlobalSearchScope.allScope(project), org.intellij.erlang.psi.ErlangNamedElement.class);
          throw new IllegalStateException("sweep-plant: the index query did not throw in dumb mode");
        }
      }
      case "plant_hang.erl" -> {
        if (op.equals("highlight")) Thread.sleep(Long.MAX_VALUE / 2);
      }
      default -> {
      }
    }
  }

  static List<JsonObject> controls(String mode, List<String> ops, ErlangRobustnessSweepTest.Acc acc) {
    List<JsonObject> r = new ArrayList<>();
    boolean highlight = ops.contains("highlight");
    if (highlight) {
      r.add(control(acc, "exception thrown by an inspection (highlight)", "plant_throw.erl", "highlight", null, "PlantInspection"));
      r.add(control(acc, "LOG.error from an inspection on a background thread (highlight)", "plant_log.erl", "highlight", "log-error", "PlantInspection"));
      r.add(slowControl(acc, "wall time over the threshold (highlight)", "plant_sleep.erl", "highlight"));
    }
    if (mode.equals("dumb") && highlight) {
      JsonObject inInspection = control(acc, "IndexNotReady from a stub index query inside an inspection in dumb mode (highlight)",
                                        "plant_index_insp.erl", "highlight", null, "IndexNotReady @");
      r.add(inInspection);
    }
    if (mode.equals("dumb")) {
      r.add(control(acc, "IndexNotReady from a stub index query in dumb mode (" + ops.get(0) + ")", "plant_index.erl", ops.get(0), "exception",
                    "IndexNotReady @ org.intellij.erlang.sweep.SweepPlants.opFault"));
    }
    if (ops.contains("folding")) r.add(control(acc, "exception thrown by a folding builder (folding)", "plant_folding.erl", "folding", null, "PlantFolding"));
    if (ops.contains("completion")) r.add(control(acc, "exception thrown by a completion contributor (completion)", "plant_completion.erl", "completion", null, "PlantCompletion"));
    for (String op : ops) {
      if (!op.endsWith("-bg")) continue;
      r.add(control(acc, "EDT-only access from a background read action (" + op + ")", "plant_bg_edt.erl", op, "exception", "EdtOnly @ org.intellij.erlang.sweep.SweepPlants.opFault"));
      r.add(control(acc, "document change in a background read action (" + op + ")", "plant_bg_write.erl", op, null, "SweepPlants.opFault"));
      JsonObject noRead = control(acc, "index query on a thread without a read action (" + op + "); proves the classification only", "plant_bg_noread.erl", op, null, "ReadAccess @");
      noRead.addProperty("required", false);
      r.add(noRead);
    }
    if (!ops.isEmpty() && !mode.equals("dumb")) {
      JsonObject slow = control(acc, "slow operation assertion on the EDT (" + ops.get(0) + ")", "plant_slowops.erl", ops.get(0), null, "SlowOps @");
      slow.addProperty("required", false);
      r.add(slow);
    }
    if (mode.equals("dumb")) {
      for (String[] c : new String[][]{{"folding", "PlantFolding", "fold"}, {"completion", "PlantCompletion", "compl"}, {"highlight", "PlantFolding", "fold"}}) {
        if (!ops.contains(c[0])) continue;
        JsonObject inExtension = control(acc, "IndexNotReady from a stub index query inside " + c[1] + " in dumb mode (" + c[0] + ")", "plant_index_" + c[2] + ".erl", c[0], null, "IndexNotReady @");
        inExtension.addProperty("required", false);
        r.add(inExtension);
      }
    }
    for (String op : ops) {
      r.add(control(acc, "exception inside the operation wrapper (" + op + ")", "plant_op.erl", op, "exception", "SweepPlants.opFault"));
      r.add(control(acc, "LOG.error on the EDT (" + op + ")", "plant_edt_log.erl", op, "log-error", "SweepPlants.opFault"));
      r.add(control(acc, "LOG.error on a pooled thread (" + op + ")", "plant_pooled_log.erl", op, "log-error", "SweepPlants.opFault"));
    }
    if (ops.contains("format")) {
      r.add(propertyControl(acc, "reformat changes the code (format)", "plant_format.erl", "format-changes-code"));
      r.add(propertyControl(acc, "second reformat changes the text (format)", "plant_format.erl", "idempotence"));
    }
    if (ops.contains("stubs")) {
      JsonObject natural = control(acc, "stub tree differs from the text (stubs); a product fault, not a plant", "natural_stub.erl", "stubs", "exception",
                                   "StubTextMismatch @ <no plugin frame> ERL_FUNCTION missing in PSI");
      natural.addProperty("required", false);
      r.add(natural);
    }
    // Negative control: the clean plant input must give no failure at all.
    boolean clean = true;
    for (JsonObject f : acc.failures) if ("plant/plant_clean.erl".equals(ErlangRobustnessSweepTest.str(f, "id"))) clean = false;
    JsonObject c = new JsonObject();
    c.addProperty("t", "control");
    c.addProperty("name", "negative control: a clean module gives no failure");
    c.addProperty("fired", clean);
    r.add(c);
    return r;
  }

  private static JsonObject control(ErlangRobustnessSweepTest.Acc acc, String name, String file, String op, String detector, String signaturePart) {
    boolean fired = false;
    String signature = null;
    for (JsonObject f : acc.failures) {
      if (!("plant/" + file).equals(ErlangRobustnessSweepTest.str(f, "id"))) continue;
      if (!op.equals(ErlangRobustnessSweepTest.str(f, "op"))) continue;
      if (detector != null && !detector.equals(ErlangRobustnessSweepTest.str(f, "detector"))) continue;
      if (!f.get("signature").getAsString().contains(signaturePart)) continue;
      fired = true;
      signature = f.get("signature").getAsString();
    }
    JsonObject c = new JsonObject();
    c.addProperty("t", "control");
    c.addProperty("name", name);
    c.addProperty("fired", fired);
    c.addProperty("signature", signature);
    return c;
  }

  private static JsonObject propertyControl(ErlangRobustnessSweepTest.Acc acc, String name, String file, String kind) {
    boolean fired = false;
    for (JsonObject p : acc.props) {
      if (("plant/" + file).equals(ErlangRobustnessSweepTest.str(p, "id")) && kind.equals(ErlangRobustnessSweepTest.str(p, "kind"))) fired = true;
    }
    JsonObject c = new JsonObject();
    c.addProperty("t", "control");
    c.addProperty("name", name);
    c.addProperty("fired", fired);
    return c;
  }

  private static JsonObject slowControl(ErlangRobustnessSweepTest.Acc acc, String name, String file, String op) {
    boolean fired = false;
    for (JsonObject d : acc.slowest) {
      if (("plant/" + file).equals(ErlangRobustnessSweepTest.str(d, "id")) && op.equals(ErlangRobustnessSweepTest.str(d, "op")) &&
          d.get("ms").getAsLong() > acc.slowMs) {
        fired = true;
      }
    }
    JsonObject c = new JsonObject();
    c.addProperty("t", "control");
    c.addProperty("name", name);
    c.addProperty("fired", fired);
    return c;
  }

  static final class PlantInspection extends LocalInspectionTool implements DumbAware {
    @Override
    public @NotNull String getShortName() {
      return "SweepPlant";
    }

    @Override
    public @NotNull String getDisplayName() {
      return "Sweep plant";
    }

    @Override
    public @NotNull String getGroupDisplayName() {
      return "Erlang";
    }

    @Override
    public boolean isEnabledByDefault() {
      return true;
    }

    @Override
    public @NotNull PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
      return new PsiElementVisitor() {
        @Override
        public void visitElement(@NotNull PsiElement element) {
          if (!(element instanceof ErlangFunction f)) return;
          String name = f.getName();
          switch (name) {
            case "plant_throw" -> throw new IllegalStateException("sweep-plant: inspection throws");
            case "plant_log" -> {
              // Only off the dispatch thread: the control also shows that the fixture runs inspections in the background.
              if (!com.intellij.openapi.application.ApplicationManager.getApplication().isDispatchThread()) {
                LOG.error("sweep-plant: inspection logs an error");
              }
            }
            case "plant_sleep" -> {
              try {
                Thread.sleep(1500);
              }
              catch (InterruptedException ignored) {
              }
            }
            case "plant_index_insp" -> stubQuery(f.getProject());
            default -> {
            }
          }
        }
      };
    }
  }

  static final class PlantPostFormat implements com.intellij.psi.impl.source.codeStyle.PostFormatProcessor {
    @Override
    public @NotNull PsiElement processElement(@NotNull PsiElement source, @NotNull com.intellij.psi.codeStyle.CodeStyleSettings settings) {
      com.intellij.psi.PsiFile file = source.getContainingFile();
      if (file != null) processText(file, file.getTextRange(), settings);
      return source;
    }

    @Override
    public @NotNull com.intellij.openapi.util.TextRange processText(@NotNull com.intellij.psi.PsiFile source,
                                                                    @NotNull com.intellij.openapi.util.TextRange rangeToReformat,
                                                                    @NotNull com.intellij.psi.codeStyle.CodeStyleSettings settings) {
      if (!source.getName().equals("plant_format.erl")) return rangeToReformat;
      Document document = source.getViewProvider().getDocument();
      if (document == null) return rangeToReformat;
      com.intellij.psi.PsiDocumentManager manager = com.intellij.psi.PsiDocumentManager.getInstance(source.getProject());
      manager.doPostponedOperationsAndUnblockDocument(document);
      document.insertString(document.getTextLength(), "%% planted\n");
      manager.commitDocument(document);
      return rangeToReformat.grown(11);
    }
  }

  static final class PlantFolding implements FoldingBuilder, DumbAware {
    @Override
    public FoldingDescriptor @NotNull [] buildFoldRegions(@NotNull ASTNode node, @NotNull Document document) {
      if (document.getText().contains("plant_folding(")) throw new IllegalStateException("sweep-plant: folding builder throws");
      if (document.getText().contains("plant_index_fold(")) stubQuery(node.getPsi().getProject());
      return FoldingDescriptor.EMPTY_ARRAY;
    }

    @Override
    public @Nullable String getPlaceholderText(@NotNull ASTNode node) {
      return "...";
    }

    @Override
    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
      return false;
    }
  }

  static final class PlantCompletion extends CompletionContributor implements DumbAware {
    @Override
    public void fillCompletionVariants(@NotNull CompletionParameters parameters, @NotNull CompletionResultSet result) {
      if (parameters.getOriginalFile().getName().equals("plant_completion.erl")) {
        throw new IllegalStateException("sweep-plant: completion contributor throws");
      }
      if (parameters.getOriginalFile().getName().equals("plant_index_compl.erl")) stubQuery(parameters.getOriginalFile().getProject());
    }
  }
}
