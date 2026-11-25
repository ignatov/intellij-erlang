package org.intellij.erlang.pbt;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.ex.FileEditorManagerEx;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.TokenType;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Property-based tests of the parser and of five inspections against erlc. Tooling, not a regression test: without
 * {@code PBT_ERL} it does nothing and passes. See README.md in this package for the knobs and the report.
 * <p>
 * Properties, with erlc as the reference:
 * <ul>
 *   <li>{@code parse}: erlc reports no syntax error, so the plugin's PSI has no {@link PsiErrorElement};</li>
 *   <li>{@code parse-reject}: erlc reports a syntax error for a mutated module, so the plugin has an error element or an
 *   error highlight (weaker, reported separately);</li>
 *   <li>{@code warnings}: the unused variables, functions and types, unbound variables and undefined functions of erlc
 *   are the ones of the plugin's inspections, keyed by kind, line and name.</li>
 * </ul>
 */
public class ErlangPropertyTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final List<String> ALL_PROPS = List.of("parse", "parse-reject", "warnings");
  static final List<String> WARNING_KINDS = List.of("unused_var", "unused_function", "unused_type", "unbound_var", "undefined_function");
  private static final Gson GSON = new GsonBuilder().disableHtmlEscaping().create();
  private static final Gson PRETTY = new GsonBuilder().disableHtmlEscaping().setPrettyPrinting().create();
  private static final String FILE_NAME = PbtGen.MODULE + ".erl";

  /** What the plugin says about a warning, by the message of each of the inspections. */
  private static final Map<Pattern, String> PLUGIN_MESSAGES = new LinkedHashMap<>();

  static {
    PLUGIN_MESSAGES.put(Pattern.compile("Unused variable '(.+)'"), "unused_var");             // ErlangUnusedVariableInspection
    PLUGIN_MESSAGES.put(Pattern.compile("Unused function '(.+)'"), "unused_function");        // ErlangUnusedFunctionInspection
    PLUGIN_MESSAGES.put(Pattern.compile("Unused type '(.+)'"), "unused_type");                // ErlangUnusedTypeInspection
    PLUGIN_MESSAGES.put(Pattern.compile("Variable '(.+)' is unbound"), "unbound_var");        // ErlangUnboundVariableInspection
    // ErlangUnresolvedFunctionInspection, and ErlangUnresolvedExportFunctionInspection for -export
    PLUGIN_MESSAGES.put(Pattern.compile("Unresolved function '?(.+?)'?"), "undefined_function");
  }

  private long mySeed;
  private List<String> myProps;
  private @Nullable String myPlant;
  private int myShrinkTests;
  private Path myWork;
  private Path myOut;
  private BufferedWriter myJournal;
  private PbtOracle myOracle;
  private final Map<String, Integer> myUsage = PbtGen.newUsage();
  private final Map<String, Prop> myStats = new LinkedHashMap<>();
  private final List<JsonObject> myDiscards = new ArrayList<>();
  private int myDiscardCount;
  private final List<JsonObject> myCrashes = new ArrayList<>();
  private long myPluginMs;
  private int myPluginCalls;
  private final List<VirtualFile> myCreated = new ArrayList<>();

  /** Planted faults of the positive controls: a fake plugin or oracle answer, one per property. */
  private @Nullable String myPlanted;

  /** Counters and signatures of one property. */
  static final class Prop {
    int checked;
    int fromMutants;
    int skipped;
    int violations;
    final Map<String, Sig> sigs = new LinkedHashMap<>();
  }

  static final class Sig {
    final String text;
    int count;
    String firstCase;
    final List<String> shrunkFiles = new ArrayList<>();
    final List<String> shrunkTexts = new ArrayList<>();
    final List<String> examples = new ArrayList<>();

    Sig(String text) {
      this.text = text;
    }
  }

  /** One input: a generated tree, or a text (a mutant) that only the text reducer can shrink. */
  record Input(String id, @Nullable PbtNode tree, String text) {
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        String release = sdkRelease();
        ErlangSdkRelease r = switch (release) {
          case "25" -> ErlangSdkRelease.V_25_0;
          case "27" -> ErlangSdkRelease.V_27_0;
          case "28" -> ErlangSdkRelease.V_28_0;
          default -> ErlangSdkRelease.V_29_0;
        };
        return ErlangSdkType.createMockSdk(sdkHome(), r);
      }
    };
  }

  private static String ourSdkRelease;
  private static String ourSdkHome;

  /** PBT_SDK_RELEASE, else the OTP release of PBT_ERL; 25, 27, 28 and 29 have a mock SDK, anything else gets 29. */
  static synchronized String sdkRelease() {
    if (ourSdkRelease == null) probeErl();
    return ourSdkRelease;
  }

  /**
   * The home of the SDK: PBT_SDK_HOME, else a copy of the mock SDK of the release with the {@code erlang.erl} of the
   * PBT_ERL installation in it. From OTP R16 on the plugin resolves the auto-imported BIFs to that file only, so with
   * the bare mock SDK every call of {@code self()} or {@code length/1} would be an unresolved function.
   */
  static synchronized String sdkHome() {
    if (ourSdkHome == null) probeErl();
    return ourSdkHome;
  }

  private static void probeErl() {
    String release = env("PBT_SDK_RELEASE");
    String erl = env("PBT_ERL");
    String root = null;
    if (erl != null) {
      try {
        Process p = new ProcessBuilder(erl, "-noshell", "-eval",
                                       "io:format(\"~s~n~s~n\", [erlang:system_info(otp_release), code:root_dir()]), halt().")
          .redirectErrorStream(true).start();
        List<String> lines = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim().lines().toList();
        p.waitFor();
        if (release == null && !lines.isEmpty()) release = lines.get(0).trim();
        if (lines.size() > 1) root = lines.get(1).trim();
      }
      catch (Exception ignored) {
      }
    }
    ourSdkRelease = release != null && List.of("25", "27", "28").contains(release) ? release : "29";
    String mock = "testData/mockSdk-R" + ourSdkRelease + "/";
    ourSdkHome = env("PBT_SDK_HOME", mock);
    if (env("PBT_SDK_HOME") != null || root == null) return;
    try (var libs = Files.list(Paths.get(root, "lib"))) {
      Path erts = libs.filter(d -> d.getFileName().toString().startsWith("erts-")).findFirst().orElse(null);
      Path erlang = erts == null ? null : erts.resolve("src/erlang.erl");
      if (erlang == null || !Files.isRegularFile(erlang)) return;
      Path home = Paths.get(env("PBT_WORK", "build/pbt/work")).toAbsolutePath().resolve("sdk-R" + ourSdkRelease);
      Path target = home.resolve("lib").resolve(erts.getFileName()).resolve("src/erlang.erl");
      Files.createDirectories(target.getParent());
      Files.copy(erlang, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
      ourSdkHome = home.toString() + "/";
    }
    catch (IOException ignored) {
    }
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ((CodeInsightTestFixtureImpl)myFixture).canChangeDocumentDuringHighlighting(true);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myOracle != null) myOracle.close();
      if (myJournal != null) myJournal.close();
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
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

  // =================================================================================================== entry point

  public void testProperties() throws Exception {
    String erl = env("PBT_ERL");
    if (erl == null) return;

    long start = System.currentTimeMillis();
    mySeed = Long.parseLong(env("PBT_SEED", String.valueOf(System.nanoTime())));
    int cases = Integer.parseInt(env("PBT_CASES", "200"));
    long budgetMs = (long)(Double.parseDouble(env("PBT_BUDGET_MINUTES", "0")) * 60_000);
    myProps = new ArrayList<>();
    for (String p : env("PBT_PROPS", String.join(",", ALL_PROPS)).split(",")) {
      if (p.isBlank()) continue;
      if (!ALL_PROPS.contains(p.trim())) throw new IllegalArgumentException("unknown property " + p);
      myProps.add(p.trim());
    }
    for (String p : ALL_PROPS) myStats.put(p, new Prop());
    myPlant = env("PBT_PLANT");
    myShrinkTests = Integer.parseInt(env("PBT_SHRINK_TESTS", "400"));
    Path report = Paths.get(env("PBT_REPORT", "build/pbt/report.json")).toAbsolutePath();
    myOut = Paths.get(env("PBT_OUT", "build/pbt/out")).toAbsolutePath();
    myWork = Paths.get(env("PBT_WORK", "build/pbt/work")).toAbsolutePath();
    Files.createDirectories(myWork);
    Files.createDirectories(myOut);
    Files.createDirectories(report.getParent());
    Path journal = Paths.get(env("PBT_JOURNAL", report + ".journal.jsonl")).toAbsolutePath();
    myJournal = Files.newBufferedWriter(journal, StandardCharsets.UTF_8);
    enableInspections();
    myOracle = new PbtOracle(erl, Paths.get("testData/pbt/pbt_oracle.erl"), 60_000);

    JsonObject run = new JsonObject();
    run.addProperty("t", "run");
    run.addProperty("seed", mySeed);
    run.addProperty("otp_release", myOracle.otpRelease());
    run.addProperty("sdk_release", sdkRelease());
    run.addProperty("sdk_home", sdkHome());
    run.addProperty("revision", revision());
    journal(run);

    String replay = env("PBT_REPLAY");
    List<String> controlFailures = new ArrayList<>();
    JsonArray controls = new JsonArray();
    int done = 0;
    boolean complete = true;
    if (myPlant != null) {
      controlFailures.addAll(controls(controls));
    }
    else if (replay != null) {
      int colon = replay.indexOf(':');
      mySeed = Long.parseLong(replay.substring(0, colon));
      int index = Integer.parseInt(replay.substring(colon + 1));
      runCase(index, true);
      done = 1;
    }
    else {
      for (int i = 0; i < cases; i++) {
        if (budgetMs > 0 && System.currentTimeMillis() - start > budgetMs) {
          complete = false;
          break;
        }
        runCase(i, false);
        done++;
      }
    }

    JsonObject out = new JsonObject();
    JsonObject meta = new JsonObject();
    meta.addProperty("seed", mySeed);
    meta.addProperty("cases_requested", cases);
    meta.addProperty("cases_run", done);
    meta.addProperty("complete", complete);
    meta.addProperty("revision", revision());
    meta.addProperty("otp_release", myOracle.otpRelease());
    meta.addProperty("sdk_release", sdkRelease());
    meta.addProperty("sdk_home", sdkHome());
    meta.addProperty("props", String.join(",", myProps));
    if (replay != null) meta.addProperty("replay", replay);
    if (myPlant != null) meta.addProperty("plant", myPlant);
    meta.addProperty("elapsed_ms", System.currentTimeMillis() - start);
    meta.addProperty("oracle_calls", myOracle.calls);
    meta.addProperty("oracle_ms", myOracle.millis);
    meta.addProperty("oracle_restarts", myOracle.restarts);
    meta.addProperty("plugin_calls", myPluginCalls);
    meta.addProperty("plugin_ms", myPluginMs);
    out.add("run", meta);
    JsonObject discards = new JsonObject();
    discards.addProperty("count", myDiscardCount);
    discards.addProperty("meaning", "generated modules that erlc rejects with a syntax error: generator bugs");
    JsonArray examples = new JsonArray();
    for (JsonObject d : myDiscards) examples.add(d);
    discards.add("examples", examples);
    out.add("discards", discards);
    JsonObject props = new JsonObject();
    for (Map.Entry<String, Prop> e : myStats.entrySet()) {
      if (!myProps.contains(e.getKey())) continue;
      props.add(e.getKey(), propJson(e.getValue()));
    }
    out.add("properties", props);
    JsonArray crashes = new JsonArray();
    for (JsonObject c : myCrashes) crashes.add(c);
    out.add("crashes", crashes);
    JsonObject usage = new JsonObject();
    for (Map.Entry<String, Integer> e : new TreeMap<>(myUsage).entrySet()) usage.addProperty(e.getKey(), e.getValue());
    JsonArray unused = new JsonArray();
    for (String c : new PbtGen(0, 0, PbtGen.newUsage()).constructs()) if (!myUsage.containsKey(c)) unused.add(c);
    JsonObject generator = new JsonObject();
    generator.add("constructs", usage);
    generator.add("never_generated", unused);
    out.add("generator", generator);
    if (myPlant != null) out.add("controls", controls);
    Files.writeString(report, PRETTY.toJson(out), StandardCharsets.UTF_8);
    System.out.println("PBT report: " + report);
    if (!controlFailures.isEmpty()) fail("controls did not behave:\n" + String.join("\n", controlFailures));
  }

  private static JsonObject propJson(Prop p) {
    JsonObject o = new JsonObject();
    o.addProperty("checked", p.checked);
    o.addProperty("from_mutants", p.fromMutants);
    o.addProperty("skipped", p.skipped);
    o.addProperty("violations", p.violations);
    JsonArray sigs = new JsonArray();
    p.sigs.values().stream().sorted((a, b) -> Integer.compare(b.count, a.count)).forEach(s -> {
      JsonObject so = new JsonObject();
      so.addProperty("signature", s.text);
      so.addProperty("count", s.count);
      so.addProperty("first_case", s.firstCase);
      JsonArray files = new JsonArray();
      for (String f : s.shrunkFiles) files.add(f);
      so.add("shrunk_files", files);
      JsonArray texts = new JsonArray();
      for (String t : s.shrunkTexts) texts.add(t);
      so.add("shrunk", texts);
      JsonArray ex = new JsonArray();
      for (String t : s.examples) ex.add(t);
      so.add("cases", ex);
      sigs.add(so);
    });
    o.add("signatures", sigs);
    return o;
  }

  private void enableInspections() {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      // The Dialyzer inspection is only a switch for an external annotator that starts the dialyzer executable.
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  private static String revision() {
    String r = env("PBT_REVISION");
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

  private synchronized void journal(JsonObject o) {
    try {
      myJournal.write(GSON.toJson(o));
      myJournal.write('\n');
      myJournal.flush();
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  // =================================================================================================== one case

  private void runCase(int index, boolean verbose) throws IOException {
    long t0 = System.currentTimeMillis();
    PbtNode tree = new PbtGen(mySeed, index, myUsage).module();
    String text = tree.print();
    String id = mySeed + ":" + index;
    if (verbose) System.out.println("=== " + id + "\n" + text);
    PbtOracle.Result erl = oracle(text);
    JsonObject rec = new JsonObject();
    rec.addProperty("t", "case");
    rec.addProperty("case", id);
    rec.addProperty("bytes", text.length());
    if (erl.hasSyntaxError()) {
      myDiscardCount++;
      PbtOracle.Item e = erl.firstSyntaxError();
      JsonObject d = new JsonObject();
      d.addProperty("case", id);
      d.addProperty("error", e.line() + ":" + e.column() + " " + e.module() + ": " + e.message());
      if (myDiscards.size() < 5) {
        d.addProperty("text", text);
        myDiscards.add(d);
      }
      d.addProperty("t", "discard");
      journal(d);
      rec.addProperty("discard", true);
    }
    else {
      checkModule(new Input(id, tree, text), erl, false, verbose);
    }
    if (myProps.contains("parse-reject")) {
      String mutant = mutate(text, new Random(mySeed * 31 + index * 131L + 7));
      if (mutant != null) {
        PbtOracle.Result merl = oracle(mutant);
        Input in = new Input(id + "#mutant", null, mutant);
        if (verbose) System.out.println("=== " + in.id + "\n" + mutant);
        if (merl.hasSyntaxError()) checkRejected(in, merl, verbose);
        else checkModule(in, merl, true, verbose);
      }
    }
    rec.addProperty("ms", System.currentTimeMillis() - t0);
    journal(rec);
  }

  /** The properties for a module that erlc accepts: {@code parse}, then {@code warnings} when the plugin parses it too. */
  private void checkModule(Input in, PbtOracle.Result erl, boolean mutant, boolean verbose) throws IOException {
    boolean wantParse = myProps.contains("parse");
    boolean wantWarnings = myProps.contains("warnings") && !mutant;
    if (!wantParse && !wantWarnings) return;
    View view = plugin(in.text, wantWarnings, false, erl);
    if (verbose) System.out.println("plugin errors: " + view.errorSigs + "\nplugin warnings: " + view.warnings + "\nerlc: " + erl.items());
    if (wantParse) {
      Prop p = myStats.get("parse");
      p.checked++;
      if (mutant) p.fromMutants++;
      if (!view.errorSigs.isEmpty()) {
        String sig = view.errorSigs.get(0);
        Set<String> errors = otherErrors(erl);
        violation("parse", sig, in, view.errorDetail, text -> {
          PbtOracle.Result r = oracle(text);
          return !r.hasSyntaxError() && errors.containsAll(otherErrors(r)) && plugin(text, false, false, null).errorSigs.contains(sig);
        });
      }
    }
    if (wantWarnings) {
      Prop p = myStats.get("warnings");
      if (!view.errorSigs.isEmpty()) {
        p.skipped++;
        return;
      }
      p.checked++;
      myLastAgreed = agreed(erl, view);
      Map<String, String> mismatches = mismatches(erl, view);
      Set<String> errors = otherErrors(erl);
      for (Map.Entry<String, String> m : mismatches.entrySet()) {
        String sig = m.getKey();
        violation("warnings", sig, in, m.getValue(), text -> {
          PbtOracle.Result r = oracle(text);
          if (r.hasSyntaxError() || !errors.containsAll(otherErrors(r))) return false;
          View v = plugin(text, true, false, r);
          return v.errorSigs.isEmpty() && mismatches(r, v).containsKey(sig);
        });
      }
    }
  }

  /**
   * The errors of erlc other than the five warning kinds, by tag. A reduced input must not have one that the original
   * has not: then erlc no longer accepts it the same way (a call of {@code is_atom()} in a guard, a module without
   * {@code -module}).
   */
  static Set<String> otherErrors(PbtOracle.Result r) {
    Set<String> tags = new java.util.TreeSet<>();
    for (PbtOracle.Item i : r.items()) if (i.error() && !WARNING_KINDS.contains(i.tag())) tags.add(i.module() + ":" + i.tag());
    return tags;
  }

  /** {@code parse-reject}: erlc reports a syntax error, so the plugin must have an error element or an error highlight. */
  private void checkRejected(Input in, PbtOracle.Result erl, boolean verbose) throws IOException {
    Prop p = myStats.get("parse-reject");
    p.checked++;
    p.fromMutants++;
    View view = plugin(in.text, false, true, null);
    if (verbose) System.out.println("plugin rejects: " + view.rejects() + "\nerlc: " + erl.firstSyntaxError());
    if (view.rejects()) return;
    PbtOracle.Item e = erl.firstSyntaxError();
    String sig = "parse-reject @ " + e.module() + " : " + normalize(e.message());
    violation("parse-reject", sig, in, e.line() + ":" + e.column() + " " + e.message(), text -> {
      PbtOracle.Result r = oracle(text);
      PbtOracle.Item first = r.firstSyntaxError();
      if (first == null) return false;
      if (!sig.equals("parse-reject @ " + first.module() + " : " + normalize(first.message()))) return false;
      return !plugin(text, false, true, null).rejects();
    });
  }

  private PbtOracle.Result oracle(String text) throws IOException {
    PbtOracle.Result r = myOracle.check(myWork.resolve(FILE_NAME), text);
    // positive control of `warnings`: a fake erlc that never reports an unused variable
    if ("warnings".equals(myPlanted)) r = r.without("unused_var");
    return r;
  }

  // =================================================================================================== violations

  @FunctionalInterface
  interface Check {
    boolean test(String text) throws IOException;
  }

  private void violation(String prop, String signature, Input in, String detail, Check stillFails) throws IOException {
    Prop p = myStats.get(prop);
    p.violations++;
    Sig s = p.sigs.computeIfAbsent(signature, Sig::new);
    s.count++;
    if (s.firstCase == null) s.firstCase = in.id;
    if (s.examples.size() < 10) s.examples.add(in.id);
    JsonObject v = new JsonObject();
    v.addProperty("t", "violation");
    v.addProperty("prop", prop);
    v.addProperty("signature", signature);
    v.addProperty("case", in.id);
    v.addProperty("detail", detail);
    if (s.shrunkTexts.size() < 2) {
      long t0 = System.currentTimeMillis();
      Predicate<String> pred = text -> {
        try {
          return stillFails.test(text);
        }
        catch (IOException e) {
          return false;
        }
      };
      Shrunk shrunk = in.tree != null ? shrinkTree(in.tree, pred) : shrinkText(in.text, pred);
      String name = String.format("%s-%08x-%d.erl", prop, signature.hashCode(), s.shrunkTexts.size() + 1);
      Path file = myOut.resolve(name);
      String header = "%% " + signature + "\n%% case " + in.id + ", " + shrunk.tests + " tests\n";
      Files.writeString(file, header + shrunk.text, StandardCharsets.UTF_8);
      s.shrunkTexts.add(shrunk.text);
      s.shrunkFiles.add(file.toString());
      v.addProperty("shrunk", shrunk.text);
      v.addProperty("shrink_tests", shrunk.tests);
      v.addProperty("shrink_ms", System.currentTimeMillis() - t0);
      v.addProperty("shrunk_still_fails", pred.test(shrunk.text));
    }
    journal(v);
  }

  record Shrunk(String text, int tests) {
  }

  /** Greedy reduction of the tree: apply the first edit that keeps the failure, then look again from that edit on. */
  private Shrunk shrinkTree(PbtNode tree, Predicate<String> fails) {
    PbtNode cur = tree;
    int tests = 0;
    Set<String> seen = new LinkedHashSet<>();
    boolean progress = true;
    while (progress && tests < myShrinkTests) {
      progress = false;
      List<PbtNode.Edit> edits = cur.edits();
      for (int i = 0; i < edits.size() && tests < myShrinkTests; i++) {
        PbtNode cand = cur.apply(edits.get(i));
        String text = cand.print();
        if (!seen.add(text)) continue;
        tests++;
        if (fails.test(text)) {
          cur = cand;
          progress = true;
          edits = cur.edits();
          i = Math.max(-1, i - 1);
        }
      }
    }
    Shrunk lines = shrinkText(cur.print(), fails, myShrinkTests / 4);
    return new Shrunk(lines.text, tests + lines.tests);
  }

  private Shrunk shrinkText(String text, Predicate<String> fails) {
    return shrinkText(text, fails, myShrinkTests);
  }

  /** Removal of chunks of lines, then of chunks of tokens, while the failure stays. */
  private Shrunk shrinkText(String text, Predicate<String> fails, int maxTests) {
    int[] tests = {0};
    List<String> lines = new ArrayList<>(List.of(text.split("(?<=\n)")));
    lines = ddmin(lines, fails, tests, maxTests);
    List<String> tokens = tokenUnits(String.join("", lines));
    tokens = ddmin(tokens, fails, tests, maxTests);
    return new Shrunk(String.join("", tokens), tests[0]);
  }

  private static List<String> ddmin(List<String> units, Predicate<String> fails, int[] tests, int maxTests) {
    List<String> cur = new ArrayList<>(units);
    for (int chunk = Integer.highestOneBit(Math.max(1, cur.size() / 2)); chunk >= 1; chunk /= 2) {
      boolean changed = true;
      while (changed && tests[0] < maxTests) {
        changed = false;
        for (int from = 0; from + chunk <= cur.size() && tests[0] < maxTests; ) {
          List<String> cand = new ArrayList<>(cur);
          cand.subList(from, from + chunk).clear();
          if (cand.isEmpty()) break;
          tests[0]++;
          if (fails.test(String.join("", cand))) {
            cur = cand;
            changed = true;
          }
          else {
            from += chunk;
          }
        }
      }
    }
    return cur;
  }

  /** The text cut into tokens, each with the whitespace after it. */
  private List<String> tokenUnits(String text) {
    List<String> out = new ArrayList<>();
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    StringBuilder cur = new StringBuilder();
    while (lexer.getTokenType() != null) {
      String tok = text.substring(lexer.getTokenStart(), lexer.getTokenEnd());
      if (lexer.getTokenType() != TokenType.WHITE_SPACE && cur.length() > 0) {
        out.add(cur.toString());
        cur.setLength(0);
      }
      cur.append(tok);
      lexer.advance();
    }
    if (cur.length() > 0) out.add(cur.toString());
    return out;
  }

  // =================================================================================================== mutants

  private static final String[] INSERTS = {",", ")", "(", "end", "->", ";", ".", "[", "]", "{", "}", "of", "when", "||", "#", "=", "|"};

  /** One token-level mutation of the text: delete, duplicate or swap a token, or insert one. */
  private @Nullable String mutate(String text, Random r) {
    List<int[]> toks = new ArrayList<>();
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    while (lexer.getTokenType() != null) {
      if (lexer.getTokenType() != TokenType.WHITE_SPACE && lexer.getTokenEnd() > lexer.getTokenStart()) {
        toks.add(new int[]{lexer.getTokenStart(), lexer.getTokenEnd()});
      }
      lexer.advance();
    }
    // the -module attribute stays: without it erlc reports something else than syntax
    int first = Math.min(toks.size() - 1, 6);
    if (toks.size() - first < 2) return null;
    int i = first + r.nextInt(toks.size() - first);
    int[] t = toks.get(i);
    String kind = new String[]{"delete", "duplicate", "swap", "insert"}[r.nextInt(4)];
    myUsage.merge("mutation." + kind, 1, Integer::sum);
    return switch (kind) {
      case "delete" -> text.substring(0, t[0]) + text.substring(t[1]);
      case "duplicate" -> text.substring(0, t[1]) + " " + text.substring(t[0], t[1]) + text.substring(t[1]);
      case "swap" -> {
        if (i + 1 >= toks.size()) yield text.substring(0, t[0]) + text.substring(t[1]);
        int[] u = toks.get(i + 1);
        yield text.substring(0, t[0]) + text.substring(u[0], u[1]) + text.substring(t[1], u[0]) + text.substring(t[0], t[1]) +
              text.substring(u[1]);
      }
      default -> text.substring(0, t[0]) + INSERTS[r.nextInt(INSERTS.length)] + " " + text.substring(t[0]);
    };
  }

  // =================================================================================================== the plugin side

  /** What the plugin says about one text. */
  static final class View {
    final List<String> errorSigs = new ArrayList<>();
    String errorDetail = "";
    boolean errorHighlight;
    /** (kind, line, name) of the plugin's warnings, with the construct around each. */
    final Map<Key, String> warnings = new LinkedHashMap<>();
    /** The construct at each erlc location, keyed by "line:column". */
    final Map<String, String> constructAt = new LinkedHashMap<>();

    boolean rejects() {
      return !errorSigs.isEmpty() || errorHighlight;
    }
  }

  record Key(String kind, int line, String name) {
    @Override
    public String toString() {
      return kind + "@" + line + ":" + name;
    }
  }

  /** Opens the text as {@code pbt.erl} in the light project; {@code locations} are the erlc items to name constructs for. */
  private View plugin(String text, boolean warnings, boolean rejectCheck, @Nullable PbtOracle.Result locations) {
    long t0 = System.currentTimeMillis();
    myPluginCalls++;
    View view = new View();
    List<Throwable> logged = new ArrayList<>();
    LoggedErrorProcessor processor = new LoggedErrorProcessor() {
      @Override
      public @NotNull Set<Action> processError(@NotNull String category, @NotNull String message, String @NotNull [] details,
                                               @Nullable Throwable t) {
        logged.add(t != null ? t : new Throwable(message));
        return EnumSet.noneOf(Action.class);
      }
    };
    try {
      LoggedErrorProcessor.executeWith(processor, (ThrowableRunnable<Throwable>)() -> {
        try {
          VirtualFile f = myFixture.getTempDirFixture().createFile(FILE_NAME);
          myCreated.add(f);
          WriteAction.run(() -> f.setBinaryContent(text.getBytes(StandardCharsets.UTF_8)));
          myFixture.configureFromExistingVirtualFile(f);
          PsiFile file = myFixture.getFile();
          Document doc = myFixture.getEditor().getDocument();
          if ("parse".equals(myPlanted) && text.contains("receive")) {
            // positive control of `parse`: a fake plugin with an error at every receive
            view.errorSigs.add("parse @ PLANTED : planted error at receive");
          }
          if (!"parse-reject".equals(myPlanted)) {
            for (PsiErrorElement e : SyntaxTraverser.psiTraverser(file).filter(PsiErrorElement.class)) {
              String sig = "parse @ " + psiName(e.getParent()) + " : " + normalize(e.getErrorDescription());
              if (view.errorSigs.isEmpty()) {
                int line = doc.getLineNumber(e.getTextRange().getStartOffset()) + 1;
                view.errorDetail = "line " + line + ": " + e.getErrorDescription();
              }
              if (!view.errorSigs.contains(sig)) view.errorSigs.add(sig);
            }
          }
          if (warnings || rejectCheck && view.errorSigs.isEmpty() && !"parse-reject".equals(myPlanted)) {
            List<HighlightInfo> infos = myFixture.doHighlighting();
            for (HighlightInfo info : infos) {
              if (info.getSeverity() == HighlightSeverity.ERROR) view.errorHighlight = true;
              String desc = info.getDescription();
              if (desc == null) continue;
              for (Map.Entry<Pattern, String> m : PLUGIN_MESSAGES.entrySet()) {
                Matcher matcher = m.getKey().matcher(desc);
                if (matcher.matches()) {
                  int offset = info.getStartOffset();
                  Key key = new Key(m.getValue(), doc.getLineNumber(offset) + 1, matcher.group(1));
                  view.warnings.putIfAbsent(key, construct(file, offset));
                  break;
                }
              }
            }
            if (locations != null) {
              for (PbtOracle.Item item : locations.items()) {
                if (item.line() < 1 || item.line() > doc.getLineCount()) continue;
                int offset = Math.min(doc.getLineStartOffset(item.line() - 1) + Math.max(0, item.column() - 1), doc.getTextLength());
                view.constructAt.put(item.line() + ":" + item.column(), construct(file, offset));
              }
            }
          }
        }
        finally {
          FileEditorManagerEx.getInstanceEx(getProject()).closeAllFiles();
          WriteAction.run(() -> {
            for (VirtualFile f : myCreated) if (f.isValid()) f.delete(this);
          });
          myCreated.clear();
          PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
          PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        }
      });
    }
    catch (Throwable t) {
      logged.add(t);
    }
    for (Throwable t : logged) crash(t);
    myPluginMs += System.currentTimeMillis() - t0;
    return view;
  }

  private void crash(Throwable t) {
    if (myCrashes.size() >= 50) return;
    JsonObject c = new JsonObject();
    c.addProperty("exception", t.getClass().getName());
    c.addProperty("message", String.valueOf(t.getMessage()));
    StackTraceElement frame = null;
    for (StackTraceElement e : t.getStackTrace()) {
      if (e.getClassName().startsWith("org.intellij.erlang.") && !e.getClassName().startsWith("org.intellij.erlang.pbt.")) {
        frame = e;
        break;
      }
    }
    if (frame != null) c.addProperty("frame", frame.toString());
    myCrashes.add(c);
    c.addProperty("t", "crash");
    journal(c);
  }

  /** The mismatches of the five warning kinds, by signature, with a detail. */
  /** The number of warnings that erlc and the plugin agree on, for the negative controls. */
  private int myLastAgreed;

  private static int agreed(PbtOracle.Result erl, View view) {
    int n = 0;
    for (Key k : erlcKeys(erl).keySet()) if (view.warnings.containsKey(k)) n++;
    return n;
  }

  private static Map<Key, PbtOracle.Item> erlcKeys(PbtOracle.Result erl) {
    Map<Key, PbtOracle.Item> ours = new LinkedHashMap<>();
    for (PbtOracle.Item i : erl.items()) {
      if (!WARNING_KINDS.contains(i.tag()) || i.name() == null) continue;
      String name = i.name();
      // the plugin names a type without its arity
      if ("unused_type".equals(i.tag()) && name.contains("/")) name = name.substring(0, name.indexOf('/'));
      ours.putIfAbsent(new Key(i.tag(), i.line(), name), i);
    }
    return ours;
  }

  private static Map<String, String> mismatches(PbtOracle.Result erl, View view) {
    Map<Key, PbtOracle.Item> ours = erlcKeys(erl);
    Map<String, String> out = new LinkedHashMap<>();
    for (Map.Entry<Key, String> e : view.warnings.entrySet()) {
      if (ours.containsKey(e.getKey())) continue;
      out.putIfAbsent("warnings:extra " + e.getKey().kind + " @ " + e.getValue(), "plugin only: " + e.getKey());
    }
    for (Map.Entry<Key, PbtOracle.Item> e : ours.entrySet()) {
      if (view.warnings.containsKey(e.getKey())) continue;
      PbtOracle.Item i = e.getValue();
      String construct = view.constructAt.getOrDefault(i.line() + ":" + i.column(), "?");
      out.putIfAbsent("warnings:missing " + e.getKey().kind + " @ " + construct, "erlc only: " + e.getKey() + " (" + i.message() + ")");
    }
    return out;
  }

  /** Constructs that name where a warning is, innermost first wins. */
  private static final List<String> CONSTRUCTS = List.of(
    "CaseExpression", "IfExpression", "ReceiveExpression", "TryExpression", "CatchExpression", "FunExpression",
    "ListComprehension", "BeginEndExpression", "MapExpression", "RecordExpression", "BinaryExpression",
    "FunctionClause", "TypeDefinition", "Specification", "RecordDefinition", "Export", "ExportTypeAttribute", "Attribute", "Function");
  private static final Map<String, String> ROLES = Map.of(
    "ArgumentDefinitionList", "args", "ClauseGuard", "guard", "LcExpression", "qualifier", "CrClause", "clause",
    "TryClause", "catch", "AfterClauseBody", "after", "FunClause", "clause", "ArgumentList", "call");

  /** The innermost construct around the offset, with the role of the element in it: {@code CaseExpression/clause}. */
  static String construct(PsiFile file, int offset) {
    PsiElement e = file.findElementAt(offset);
    String role = null;
    for (PsiElement p = e; p != null && !(p instanceof PsiFile); p = p.getParent()) {
      String name = psiName(p);
      if (role == null && ROLES.containsKey(name)) role = ROLES.get(name);
      if (role == null && "AssignmentExpression".equals(name) && p.getFirstChild() != null &&
          p.getFirstChild().getTextRange().contains(offset)) {
        role = "match";
      }
      if (CONSTRUCTS.contains(name)) return role == null ? name : name + "/" + role;
    }
    return "File";
  }

  static String psiName(@Nullable PsiElement e) {
    if (e == null) return "null";
    if (e instanceof PsiFile) return "File";
    String n = e.getClass().getSimpleName();
    if (n.startsWith("Erlang")) n = n.substring("Erlang".length());
    if (n.endsWith("Impl")) n = n.substring(0, n.length() - "Impl".length());
    return n;
  }

  private static final Set<String> KEYWORDS = Set.of("after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor",
                                                     "case", "catch", "cond", "div", "end", "fun", "if", "let", "not", "of", "or",
                                                     "orelse", "receive", "rem", "try", "when", "xor", "maybe", "else");
  private static final Pattern QUOTED = Pattern.compile("'([^']*)'|(?<=before: )(\\S+)");

  /** An error text without the names and numbers in it, so that one bug has one signature. */
  static String normalize(String s) {
    Matcher m = QUOTED.matcher(s);
    StringBuilder sb = new StringBuilder();
    while (m.find()) {
      String tok = m.group(1) != null ? m.group(1) : m.group(2);
      String cls = tokenClass(tok);
      String rep = cls == null ? m.group() : m.group(1) != null ? "'" + cls + "'" : cls;
      m.appendReplacement(sb, Matcher.quoteReplacement(rep));
    }
    m.appendTail(sb);
    return sb.toString();
  }

  private static @Nullable String tokenClass(String tok) {
    if (tok.matches("[A-Z_][A-Za-z0-9_@]*")) return "<var>";
    if (tok.matches("[a-z][A-Za-z0-9_@]*")) return KEYWORDS.contains(tok) ? null : "<atom>";
    if (tok.matches("'.*'")) return "<atom>";
    if (tok.matches("-?[0-9][0-9_#a-zA-Z.]*")) return "<number>";
    if (tok.matches("\".*")) return "<string>";
    if (tok.matches("\\$.+")) return "<char>";
    return null;
  }

  // =================================================================================================== controls

  /**
   * The negative control: the modules of {@code testData/pbt/controls}, where erlc and the plugin agree, give no
   * violation. The positive controls: with a planted fault per property, a violation shows up and shrinks to a few lines.
   */
  private List<String> controls(JsonArray out) throws IOException {
    List<String> failures = new ArrayList<>();
    Path dir = Paths.get("testData/pbt/controls");
    List<Path> files = new ArrayList<>();
    try (var stream = Files.list(dir)) {
      stream.filter(p -> p.toString().endsWith(".erl")).sorted().forEach(files::add);
    }
    for (Path f : files) {
      String text = Files.readString(f);
      Map<String, Integer> before = violationCounts();
      myLastAgreed = 0;
      PbtOracle.Result erl = oracle(text);
      Input in = new Input("control/" + f.getFileName(), null, text);
      if (erl.hasSyntaxError()) checkRejected(in, erl, false);
      else checkModule(in, erl, false, false);
      Map<String, Integer> after = violationCounts();
      JsonObject c = new JsonObject();
      c.addProperty("control", "negative " + f.getFileName());
      c.addProperty("erlc_syntax_error", erl.hasSyntaxError());
      // a control may say how many warnings both sides must report, so that it cannot pass by both saying nothing
      Matcher agree = Pattern.compile("%% agree: (\\d+)").matcher(text);
      int want = agree.find() ? Integer.parseInt(agree.group(1)) : 0;
      c.addProperty("agreed_warnings", myLastAgreed);
      boolean ok = after.equals(before) && myLastAgreed >= want;
      c.addProperty("ok", ok);
      out.add(c);
      if (!after.equals(before)) failures.add("negative control " + f.getFileName() + " has violations: " + diff(before, after));
      if (myLastAgreed < want) failures.add("negative control " + f.getFileName() + ": " + myLastAgreed + " agreed warnings, want " + want);
    }
    for (String prop : myProps) {
      myPlanted = prop;
      Prop p = myStats.get(prop);
      int before = p.violations;
      int index = 0;
      while (p.violations == before && index < 40) runCase(index++, false);
      myPlanted = null;
      JsonObject c = new JsonObject();
      c.addProperty("control", "positive " + prop);
      c.addProperty("cases", index);
      Sig planted = null;
      for (Sig s : p.sigs.values()) if (!s.shrunkTexts.isEmpty() && s.count > 0 && isPlanted(prop, s.text)) planted = s;
      String problem = null;
      if (p.violations == before || planted == null) {
        problem = "no planted violation in " + index + " cases";
      }
      else {
        String shrunk = planted.shrunkTexts.get(0);
        long lines = shrunk.lines().filter(l -> !l.isBlank()).count();
        c.addProperty("signature", planted.text);
        c.addProperty("shrunk_lines", lines);
        c.addProperty("shrunk", shrunk);
        if (lines > 8) problem = "the planted violation shrank to " + lines + " lines, more than 8";
      }
      c.addProperty("ok", problem == null);
      out.add(c);
      if (problem != null) failures.add("positive control " + prop + ": " + problem);
    }
    return failures;
  }

  private static boolean isPlanted(String prop, String sig) {
    return switch (prop) {
      case "parse" -> sig.contains("PLANTED");
      case "warnings" -> sig.startsWith("warnings:extra unused_var");
      default -> sig.startsWith("parse-reject");
    };
  }

  private Map<String, Integer> violationCounts() {
    Map<String, Integer> m = new TreeMap<>();
    for (Map.Entry<String, Prop> e : myStats.entrySet()) {
      for (Sig s : e.getValue().sigs.values()) m.put(e.getKey() + " " + s.text, s.count);
    }
    return m;
  }

  private static String diff(Map<String, Integer> before, Map<String, Integer> after) {
    List<String> d = new ArrayList<>();
    for (Map.Entry<String, Integer> e : after.entrySet()) {
      if (!e.getValue().equals(before.get(e.getKey()))) d.add(e.getKey());
    }
    Collections.sort(d);
    return String.join("; ", d);
  }
}
