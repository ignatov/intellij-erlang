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
import com.intellij.psi.PsiReference;
import com.intellij.psi.TokenType;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.LoggedErrorProcessor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.ErlangMaybeExpression;
import org.intellij.erlang.psi.ErlangQVar;
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
import java.util.HashSet;
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
 * Property-based tests of the parser and of six inspections against erlc. Tooling, not a regression test: without
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
    int knownViolations;
    final Map<String, Sig> sigs = new LinkedHashMap<>();
  }

  static final class Sig {
    final String text;
    /** The label of PBT_KNOWN for this signature, or null when it is new. */
    final @Nullable String known;
    int count;
    String firstCase;
    final List<String> shrunkFiles = new ArrayList<>();
    final List<String> shrunkTexts = new ArrayList<>();
    final List<String> examples = new ArrayList<>();

    Sig(String text, @Nullable String known) {
      this.text = text;
      this.known = known;
    }
  }

  /** PBT_KNOWN: the labels of exact signatures, and of patterns with {@code *}, in the order of the file. */
  private final Map<String, String> myKnownExact = new LinkedHashMap<>();
  private final List<Map.Entry<Pattern, String>> myKnownPatterns = new ArrayList<>();

  /** Reads lines {@code signature<TAB>label}; blank lines and lines starting with {@code #} are skipped. */
  private void loadKnown(@Nullable String file) throws IOException {
    if (file == null) return;
    for (String line : Files.readAllLines(Paths.get(file), StandardCharsets.UTF_8)) {
      if (line.isBlank() || line.startsWith("#")) continue;
      int tab = line.indexOf('\t');
      if (tab < 0) throw new IllegalArgumentException("PBT_KNOWN: no tab in line: " + line);
      String sig = line.substring(0, tab);
      String label = line.substring(tab + 1).trim();
      if (sig.contains("*")) {
        String[] parts = sig.split("\\*", -1);
        StringBuilder re = new StringBuilder();
        for (int i = 0; i < parts.length; i++) {
          if (i > 0) re.append(".*");
          re.append(Pattern.quote(parts[i]));
        }
        myKnownPatterns.add(Map.entry(Pattern.compile(re.toString()), label));
      }
      else {
        myKnownExact.put(sig, label);
      }
    }
  }

  /** The label of a known signature: an exact line first, else the first pattern that matches the whole signature. */
  @Nullable String known(String signature) {
    String label = myKnownExact.get(signature);
    if (label != null) return label;
    for (Map.Entry<Pattern, String> e : myKnownPatterns) if (e.getKey().matcher(signature).matches()) return e.getValue();
    return null;
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
        // the VFS may keep the children of the home from an earlier run, before the stubs of lists and maps were there
        VirtualFile home = com.intellij.openapi.vfs.LocalFileSystem.getInstance().refreshAndFindFileByPath(sdkHome());
        if (home != null) com.intellij.openapi.vfs.VfsUtil.markDirtyAndRefresh(false, true, true, home);
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
      List<Path> dirs = libs.toList();
      Path erts = dirs.stream().filter(d -> d.getFileName().toString().startsWith("erts-")).findFirst().orElse(null);
      Path erlang = erts == null ? null : erts.resolve("src/erlang.erl");
      if (erlang == null || !Files.isRegularFile(erlang)) return;
      Path home = Paths.get(env("PBT_WORK", "build/pbt/work")).toAbsolutePath().resolve("sdk-R" + ourSdkRelease);
      Path target = home.resolve("lib").resolve(erts.getFileName()).resolve("src/erlang.erl");
      Files.createDirectories(target.getParent());
      Files.copy(erlang, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
      // stubs of the stdlib functions that the generator calls remotely and imports, so that they resolve; stubs and not
      // the real files, whose parsing for the index would add to the coverage of the parser
      Path stdlib = home.resolve("lib/stdlib-0/src");
      Files.createDirectories(stdlib);
      Files.writeString(stdlib.resolve("lists.erl"), "-module(lists).\n-export([reverse/1, member/2, seq/2]).\n" +
                                                     "reverse(_) -> [].\nmember(_, _) -> false.\nseq(_, _) -> [].\n");
      Files.writeString(stdlib.resolve("maps.erl"), "-module(maps).\n-export([get/2, put/3, size/1]).\n" +
                                                    "get(_, _) -> ok.\nput(_, _, _) -> #{}.\nsize(_) -> 0.\n");
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
    loadKnown(env("PBT_KNOWN"));
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
    else if (env("PBT_FILES") != null) {
      for (Path f : erlFiles(Paths.get(env("PBT_FILES")))) {
        String text = Files.readString(f);
        System.out.println("=== " + f + "\n" + text);
        PbtOracle.Result answer = oracle(text);
        Input in = new Input("file/" + f.getFileName(), null, text);
        if (answer.hasSyntaxError()) checkRejected(in, answer, true);
        else checkModule(in, answer, false, true);
        done++;
      }
    }
    else {
      List<Integer> indices = indices(env("PBT_INDICES"), cases);
      for (int i : indices) {
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
    if (env("PBT_KNOWN") != null) meta.addProperty("known_file", env("PBT_KNOWN"));
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
    JsonArray off = new JsonArray();
    for (String c : PbtGen.OFF) if (!c.isBlank() && !"null".equals(c)) off.add(c);
    generator.add("off", off);
    out.add("generator", generator);
    if (myPlant != null) out.add("controls", controls);
    Files.writeString(report, PRETTY.toJson(out), StandardCharsets.UTF_8);
    System.out.println("PBT report: " + report);
    if (!controlFailures.isEmpty()) fail("controls did not behave:\n" + String.join("\n", controlFailures));
  }

  /** PBT_INDICES ({@code 3,10-12} or {@code @file} with such lines), else 0 to {@code cases - 1}. */
  static List<Integer> indices(@Nullable String spec, int cases) throws IOException {
    List<Integer> out = new ArrayList<>();
    if (spec == null) {
      for (int i = 0; i < cases; i++) out.add(i);
      return out;
    }
    String text = spec.startsWith("@") ? Files.readString(Paths.get(spec.substring(1))) : spec;
    for (String part : text.split("[,\\s]+")) {
      if (part.isBlank()) continue;
      int dash = part.indexOf('-');
      int from = Integer.parseInt(dash < 0 ? part : part.substring(0, dash));
      int to = dash < 0 ? from : Integer.parseInt(part.substring(dash + 1));
      for (int i = from; i <= to; i++) out.add(i);
    }
    return out;
  }

  private static JsonObject propJson(Prop p) {
    JsonObject o = new JsonObject();
    o.addProperty("checked", p.checked);
    o.addProperty("from_mutants", p.fromMutants);
    o.addProperty("skipped", p.skipped);
    o.addProperty("violations", p.violations);
    o.addProperty("known_violations", p.knownViolations);
    o.addProperty("new_violations", p.violations - p.knownViolations);
    // the new signatures and the known ones (PBT_KNOWN) apart, so that the new ones stand out
    JsonArray sigs = new JsonArray();
    JsonArray knownSigs = new JsonArray();
    p.sigs.values().stream().sorted((a, b) -> Integer.compare(b.count, a.count)).forEach(s -> {
      JsonObject so = new JsonObject();
      so.addProperty("signature", s.text);
      if (s.known != null) so.addProperty("known", s.known);
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
      (s.known != null ? knownSigs : sigs).add(so);
    });
    o.addProperty("new_signature_count", sigs.size());
    o.addProperty("known_signature_count", knownSigs.size());
    o.add("signatures", sigs);
    o.add("known_signatures", knownSigs);
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
    for (PbtOracle.Item i : r.items()) {
      if (!i.error()) continue;
      if (!WARNING_KINDS.contains(i.tag())) tags.add(i.module() + ":" + i.tag());
      // `_` used as an expression is an artifact of the token reduction, not a variable of the module
      else if ("_".equals(i.name())) tags.add(i.module() + ":" + i.tag() + " _");
    }
    return tags;
  }

  /** {@code parse-reject}: erlc reports a syntax error, so the plugin must have an error element or an error highlight. */
  private void checkRejected(Input in, PbtOracle.Result erl, boolean verbose) throws IOException {
    Prop p = myStats.get("parse-reject");
    p.checked++;
    p.fromMutants++;
    View view = plugin(in.text, false, true, erl);
    if (verbose) System.out.println("plugin rejects: " + view.rejects() + "\nerlc: " + erl.firstSyntaxError());
    if (view.rejects()) return;
    PbtOracle.Item e = erl.firstSyntaxError();
    String sig = rejectSignature(e, view);
    int errors = syntaxErrorCount(erl);
    violation("parse-reject", sig, in, e.line() + ":" + e.column() + " " + e.message(), text -> {
      PbtOracle.Result r = oracle(text);
      PbtOracle.Item first = r.firstSyntaxError();
      // the same first error, and no new one in another form
      if (first == null || syntaxErrorCount(r) > errors) return false;
      if (!sig.startsWith("parse-reject @ " + first.module() + " : " + normalize(first.message()) + " @ ")) return false;
      View v = plugin(text, false, true, r);
      return !v.rejects() && sig.equals(rejectSignature(first, v));
    });
  }

  /** The erlc module and message of the first syntax error, and the PSI parent of the plugin at its place. */
  private static String rejectSignature(PbtOracle.Item e, View view) {
    return "parse-reject @ " + e.module() + " : " + normalize(e.message()) + " @ " + view.syntaxErrorAt;
  }

  private static int syntaxErrorCount(PbtOracle.Result r) {
    int n = 0;
    for (PbtOracle.Item i : r.items()) if (i.syntax()) n++;
    return n;
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
    Sig s = p.sigs.computeIfAbsent(signature, sig -> new Sig(sig, known(sig)));
    if (s.known != null) p.knownViolations++;
    s.count++;
    if (s.firstCase == null) s.firstCase = in.id;
    if (s.examples.size() < 10) s.examples.add(in.id);
    JsonObject v = new JsonObject();
    v.addProperty("t", "violation");
    v.addProperty("prop", prop);
    v.addProperty("signature", signature);
    v.addProperty("case", in.id);
    v.addProperty("detail", detail);
    if (s.known != null) v.addProperty("known", s.known);
    // a known signature is counted and marked, not shrunk again
    if (s.known == null && s.shrunkTexts.size() < 2) {
      long t0 = System.currentTimeMillis();
      Predicate<String> pred = text -> {
        try {
          return stillFails.test(text);
        }
        catch (IOException e) {
          return false;
        }
      };
      // a text has no tree to reduce, so its reduction by lines and tokens gets more tests
      Shrunk shrunk = in.tree != null ? shrinkTree(in.tree, pred) : shrinkText(in.text, pred, myShrinkTests * 3);
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

  /**
   * Reduction of a text: chunks of lines, then rounds of the structured token reduction and of chunks of tokens, while
   * the failure stays and the budget lasts. Every distinct candidate is tested once.
   */
  private Shrunk shrinkText(String text, Predicate<String> fails, int maxTests) {
    int[] tests = {0};
    Set<String> seen = new HashSet<>();
    seen.add(text);
    Predicate<String> once = t -> {
      if (!seen.add(t)) return false;
      tests[0]++;
      return fails.test(t);
    };
    List<String> lines = new ArrayList<>(List.of(text.split("(?<=\n)")));
    lines = ddmin(lines, once, tests, maxTests);
    List<String> tokens = tokenUnits(String.join("", lines));
    while (tests[0] < maxTests) {
      int before = String.join("", tokens).length();
      tokens = reduceStructure(tokens, once, tests, maxTests);
      tokens = ddmin(tokens, once, tests, maxTests);
      if (String.join("", tokens).length() >= before) break;
    }
    return new Shrunk(String.join("", tokens), tests[0]);
  }

  /** Removal of chunks of units, halves first; {@code fails} counts the tests. */
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

  private static final Set<String> OPEN = Set.of("(", "[", "{", "<<", "begin", "case", "if", "receive", "try", "maybe");
  private static final Set<String> CLOSE = Set.of(")", "]", "}", ">>", "end");
  private static final Set<String> SEPARATORS = Set.of(",", ";");

  /**
   * Hierarchical delta debugging on the tokens: remove a whole form, a bracketed group or {@code case ... end} block
   * with a separator next to it, an item between separators of a group, the inside of a group; replace a group by
   * {@code ok}. Largest candidates first; after each success the candidates are made again from the new text.
   */
  private static List<String> reduceStructure(List<String> units, Predicate<String> fails, int[] tests, int maxTests) {
    List<String> cur = units;
    boolean progress = true;
    while (progress && tests[0] < maxTests) {
      progress = false;
      for (List<String> cand : structureCandidates(cur)) {
        if (tests[0] >= maxTests) break;
        if (cand.isEmpty()) continue;
        if (fails.test(String.join("", cand))) {
          cur = cand;
          progress = true;
          break;
        }
      }
    }
    return cur;
  }

  private static List<List<String>> structureCandidates(List<String> units) {
    int n = units.size();
    String[] tok = new String[n];
    for (int i = 0; i < n; i++) tok[i] = units.get(i).strip();
    // groups by matching brackets and keyword blocks; a `fun` opens a block only when a clause follows it
    List<int[]> groups = new ArrayList<>();
    List<int[]> items = new ArrayList<>();
    List<int[]> forms = new ArrayList<>();
    java.util.ArrayDeque<int[]> stack = new java.util.ArrayDeque<>();  // {open index, start of the current item}
    int formStart = 0;
    int topItem = 0;
    for (int i = 0; i < n; i++) {
      String t = tok[i];
      boolean funBlock = "fun".equals(t) && i + 1 < n &&
                         ("(".equals(tok[i + 1]) || i + 2 < n && tok[i + 1].matches("[A-Z_]\\w*") && "(".equals(tok[i + 2]));
      if (OPEN.contains(t) || funBlock) {
        stack.push(new int[]{i, i + 1});
      }
      else if (CLOSE.contains(t) && !stack.isEmpty()) {
        int[] g = stack.pop();
        if (g[1] < i) items.add(new int[]{g[1], i - 1});
        groups.add(new int[]{g[0], i});
      }
      else if (SEPARATORS.contains(t) || "->".equals(t) || "of".equals(t) || "||".equals(t)) {
        int[] g = stack.peek();
        if (g != null) {
          if (SEPARATORS.contains(t) && g[1] < i) items.add(new int[]{g[1], i});
          g[1] = i + 1;
        }
        else if (SEPARATORS.contains(t)) {
          if (topItem < i) items.add(new int[]{topItem, i});
          topItem = i + 1;
        }
      }
      else if (".".equals(t) && stack.isEmpty()) {
        // the last clause of a function, without the dot
        if (topItem > formStart && topItem < i) items.add(new int[]{topItem, i - 1});
        forms.add(new int[]{formStart, i});
        formStart = i + 1;
        topItem = i + 1;
      }
    }
    List<int[]> removals = new ArrayList<>();  // {from, to inclusive}
    List<int[]> replacements = new ArrayList<>();
    removals.addAll(forms);
    for (int[] g : groups) {
      int o = g[0], c = g[1];
      if (c + 1 < n && SEPARATORS.contains(tok[c + 1])) removals.add(new int[]{o, c + 1});
      if (o > 0 && SEPARATORS.contains(tok[o - 1])) removals.add(new int[]{o - 1, c});
      removals.add(new int[]{o, c});
      if (c - o > 1 && !"end".equals(tok[c])) removals.add(new int[]{o + 1, c - 1});
      replacements.add(new int[]{o, c});
    }
    for (int[] it : items) {
      removals.add(it);
      // an item without its separator, or with the separator before it
      if (SEPARATORS.contains(tok[it[1]]) && it[1] > it[0]) removals.add(new int[]{it[0], it[1] - 1});
      if (it[0] > 0 && SEPARATORS.contains(tok[it[0] - 1])) removals.add(new int[]{it[0] - 1, it[1]});
    }
    List<List<String>> out = new ArrayList<>();
    removals.sort((a, b) -> Integer.compare(b[1] - b[0], a[1] - a[0]));
    for (int[] r : removals) {
      List<String> cand = new ArrayList<>(units);
      cand.subList(r[0], r[1] + 1).clear();
      out.add(cand);
    }
    replacements.sort((a, b) -> Integer.compare(b[1] - b[0], a[1] - a[0]));
    for (int[] r : replacements) {
      if (r[1] - r[0] < 1) continue;
      List<String> cand = new ArrayList<>(units.subList(0, r[0]));
      String last = units.get(r[1]);
      cand.add("ok" + last.substring(last.stripTrailing().length()));
      cand.addAll(units.subList(r[1] + 1, n));
      out.add(cand);
    }
    return out;
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
    /** The PSI parent of the token at the first syntax error of erlc, for the signature of {@code parse-reject}. */
    String syntaxErrorAt = "?";
    /** (kind, line, column, name) of the plugin's warnings, with the place around each and what the plugin thinks of it. */
    final Map<Key, String> warnings = new LinkedHashMap<>();
    /** The place at each erlc item of the five kinds, with what the plugin thinks of it, keyed by "kind@line:column". */
    final Map<String, String> constructAt = new LinkedHashMap<>();

    boolean rejects() {
      return !errorSigs.isEmpty() || errorHighlight;
    }
  }

  /**
   * A warning of one of the five kinds. The column is there only for variables; erlc and the plugin put the warnings
   * about functions and types at different columns, so for them it is 0.
   */
  record Key(String kind, int line, int column, String name) {
    static Key of(String kind, int line, int column, String name) {
      return new Key(kind, line, kind.endsWith("_var") ? column : 0, name);
    }

    @Override
    public String toString() {
      return kind + "@" + line + ":" + column + ":" + name;
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
                  int line = doc.getLineNumber(offset);
                  Key key = Key.of(m.getValue(), line + 1, offset - doc.getLineStartOffset(line) + 1, matcher.group(1));
                  if (!view.warnings.containsKey(key)) {
                    view.warnings.put(key, warnings ? place(file, offset) + relation(file, offset, key.kind, true) + erlcQuirk(file, offset, key.kind)
                                                    : "");
                  }
                  break;
                }
              }
            }
            if (warnings && locations != null) {
              for (PbtOracle.Item item : locations.items()) {
                if (!WARNING_KINDS.contains(item.tag())) continue;
                int offset = varOffset(file, offsetOf(doc, item), item.name());
                if (offset < 0) continue;
                view.constructAt.put(item.tag() + "@" + item.line() + ":" + item.column(),
                                     place(file, offset) + relation(file, offset, item.tag(), false));
              }
            }
          }
          PbtOracle.Item syntax = locations == null ? null : locations.firstSyntaxError();
          if (rejectCheck && syntax != null) {
            int offset = offsetOf(doc, syntax);
            PsiElement at = offset < 0 || offset >= doc.getTextLength() ? null : file.findElementAt(offset);
            view.syntaxErrorAt = at == null ? "EOF" : psiName(at.getParent());
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

  /** The offset of the line and column of an erlc item, or -1 when it is not in the text. */
  private static int offsetOf(Document doc, PbtOracle.Item item) {
    if (item.line() < 1 || item.line() > doc.getLineCount()) return -1;
    return Math.min(doc.getLineStartOffset(item.line() - 1) + Math.max(0, item.column() - 1), doc.getTextLength());
  }

  /**
   * erlc puts an unused name of a named fun at the {@code fun} keyword: then the offset of the name in that fun, else
   * the offset itself.
   */
  private static int varOffset(PsiFile file, int offset, @Nullable String name) {
    if (offset < 0 || name == null) return offset;
    PsiElement leaf = file.findElementAt(offset);
    if (leaf == null || !"fun".equals(leaf.getText()) || PsiTreeUtil.getParentOfType(leaf, ErlangQVar.class, false) != null) return offset;
    for (ErlangQVar q : SyntaxTraverser.psiTraverser(leaf.getParent()).filter(ErlangQVar.class)) {
      if (q.getText().equals(name)) return q.getTextRange().getStartOffset();
    }
    return offset;
  }

  /**
   * erlc 29 reports no unused variable at all in a function with a {@code maybe} inside a clause of a case, if, receive
   * or try ({@code f() -> X = 1, case ok of _ -> maybe ok end end.}): the plugin's unused variables there say so.
   */
  static String erlcQuirk(PsiFile file, int offset, String kind) {
    if (!"unused_var".equals(kind)) return "";
    PsiElement fn = PsiTreeUtil.getParentOfType(file.findElementAt(offset), ErlangFunction.class);
    if (fn == null) return "";
    for (PsiElement m : SyntaxTraverser.psiTraverser(fn).filter(ErlangMaybeExpression.class)) {
      for (PsiElement p = m.getParent(); p != null && p != fn; p = p.getParent()) {
        String n = psiName(p);
        if (n.equals("CrClause") || n.equals("IfClause") || n.equals("TryClause") || n.equals("TryExpressionsClause") || n.equals("AfterClauseBody")) {
          return " [erlc: maybe in a clause]";
        }
      }
    }
    return "";
  }

  /** The number of warnings that erlc and the plugin agree on, for the negative controls. */
  private int myLastAgreed;

  private static int agreed(PbtOracle.Result erl, View view) {
    Pairing p = pair(erl, view);
    return erlcKeys(erl).size() - p.missing.size();
  }

  private static Map<Key, PbtOracle.Item> erlcKeys(PbtOracle.Result erl) {
    Map<Key, PbtOracle.Item> ours = new LinkedHashMap<>();
    for (PbtOracle.Item i : erl.items()) {
      if (!WARNING_KINDS.contains(i.tag()) || i.name() == null) continue;
      String name = i.name();
      // the plugin names a type without its arity
      if ("unused_type".equals(i.tag()) && name.contains("/")) name = name.substring(0, name.indexOf('/'));
      ours.putIfAbsent(Key.of(i.tag(), i.line(), i.column(), name), i);
    }
    return ours;
  }

  /** The warnings of the plugin and of erlc that have no partner on the other side. */
  record Pairing(List<Key> extra, Map<Key, PbtOracle.Item> missing) {
  }

  /**
   * Pairs the warnings: first with the same key, then the rest of one kind, line and name at another column, one to
   * one. erlc puts an unused name of a named fun at the {@code fun} keyword, the plugin at the name.
   */
  private static Pairing pair(PbtOracle.Result erl, View view) {
    Map<Key, PbtOracle.Item> missing = new LinkedHashMap<>(erlcKeys(erl));
    List<Key> extra = new ArrayList<>();
    for (Key k : view.warnings.keySet()) if (missing.remove(k) == null) extra.add(k);
    for (java.util.Iterator<Key> it = extra.iterator(); it.hasNext(); ) {
      Key k = it.next();
      Key partner = null;
      for (Key m : missing.keySet()) {
        if (m.kind.equals(k.kind) && m.line == k.line && m.name.equals(k.name)) {
          partner = m;
          break;
        }
      }
      if (partner != null) {
        missing.remove(partner);
        it.remove();
      }
    }
    // erlc reports an unbound variable at its first use only, the plugin at every use
    Set<String> unbound = new HashSet<>();
    for (Key m : erlcKeys(erl).keySet()) if ("unbound_var".equals(m.kind)) unbound.add(m.name);
    extra.removeIf(k -> "unbound_var".equals(k.kind) && unbound.contains(k.name));
    return new Pairing(extra, missing);
  }

  /** The mismatches of the five warning kinds, by signature, with a detail. */
  private static Map<String, String> mismatches(PbtOracle.Result erl, View view) {
    Pairing p = pair(erl, view);
    Map<String, String> out = new LinkedHashMap<>();
    for (Key k : p.extra) out.putIfAbsent("warnings:extra " + k.kind + " @ " + view.warnings.get(k), "plugin only: " + k);
    for (Map.Entry<Key, PbtOracle.Item> e : p.missing.entrySet()) {
      PbtOracle.Item i = e.getValue();
      String construct = view.constructAt.getOrDefault(i.tag() + "@" + i.line() + ":" + i.column(), "?");
      out.putIfAbsent("warnings:missing " + e.getKey().kind + " @ " + construct, "erlc only: " + e.getKey() + " (" + i.message() + ")");
    }
    return out;
  }

  /** The constructs that name the place of a warning; the innermost one wins. */
  private static final Set<String> SCOPES = Set.of(
    "CaseExpression", "IfExpression", "ReceiveExpression", "TryExpression", "MaybeExpression", "CatchExpression", "FunExpression",
    "ListComprehension", "BeginEndExpression", "FunctionClause", "TypeDefinition", "Specification", "RecordDefinition", "Export",
    "ExportTypeAttribute", "Attribute", "Function");
  /** The data constructors that a place mentions after its scope: {@code FunctionClause/head in Binary}. */
  private static final Map<String, String> DATA = Map.of(
    "BinaryExpression", "Binary", "MapExpression", "Map", "MapTuple", "Map", "RecordExpression", "Record", "RecordTuple", "Record");

  static String place(PsiFile file, int offset) {
    PsiElement e = file.findElementAt(offset);
    return e == null ? "EOF" : place(e);
  }

  /**
   * The place of an element: the innermost scope around it, the part of the scope's clause it is in, and the innermost
   * data constructor between them. Parts: {@code head} (the patterns of a clause), {@code name} (of a named fun),
   * {@code guard}, {@code body}, {@code subject} (of a case), {@code timeout} (of an after), {@code gen-pattern},
   * {@code gen-expr}, {@code filter} and {@code template} (of a comprehension). A try names its parts {@code body},
   * {@code of-head}, {@code catch-head}, {@code after} and so on. A head inside a pattern {@code P = Q} gets
   * {@code -alias}, a body inside the left side of a match {@code -match}. Example: {@code TryExpression/catch-head-alias}.
   */
  static String place(PsiElement e) {
    String data = null;
    String part = null;
    String clause = null;
    boolean assign = false;
    boolean lhs = false;
    PsiElement c = null;
    for (PsiElement p = e; p != null && !(p instanceof PsiFile); c = p, p = p.getParent()) {
      String name = psiName(p);
      String cn = c == null ? "" : psiName(c);
      if (part == null) {
        if (data == null && DATA.containsKey(name)) data = DATA.get(name);
        if ("AssignmentExpression".equals(name) || "MaybeMatchExpression".equals(name)) {
          assign = true;
          if (c != null && c == p.getFirstChild()) lhs = true;
        }
        part = switch (name) {
          case "FunctionClause", "CrClause", "TryClause", "FunClause", "IfClause" -> switch (cn) {
            case "ArgumentDefinitionList" -> "head";
            case "ArgumentDefinition" -> "FunClause".equals(name) ? "name" : "head";
            case "ClauseGuard", "Guard" -> "guard";
            case "ClauseBody" -> "body";
            default -> null;
          };
          case "AfterClauseBody" -> "ClauseBody".equals(cn) ? "body" : "timeout";
          case "LcExpression" -> "ArgumentDefinition".equals(cn) ? "gen-pattern"
                                                                 : PsiTreeUtil.getChildOfType(p, ErlangArgumentDefinition.class) != null
                                                                   ? "gen-expr" : "filter";
          case "ListComprehension" -> "template";
          case "CaseExpression" -> "subject";
          case "TryExpression" -> {
            if (!"TryExpressionsClause".equals(cn)) yield null;
            PsiElement before = PsiTreeUtil.skipWhitespacesAndCommentsBackward(c);
            yield before != null && "after".equals(before.getText()) ? "after" : "body";
          }
          case "BeginEndExpression", "CatchExpression" -> "body";
          case "MaybeExpression" -> "MaybeMatchExprs".equals(cn) ? "body" : null;
          default -> null;
        };
        if (part != null) clause = name;
      }
      if (!SCOPES.contains(name)) continue;
      StringBuilder sb = new StringBuilder(name);
      if (part != null) {
        String prefix = "";
        if ("TryClause".equals(clause)) prefix = "catch-";
        else if ("CrClause".equals(clause) && "TryExpression".equals(name)) prefix = "of-";
        else if ("CrClause".equals(clause) && "MaybeExpression".equals(name)) prefix = "else-";
        else if ("AfterClauseBody".equals(clause)) prefix = "after-";
        sb.append('/').append(prefix).append(part);
        boolean pattern = part.equals("head") || part.equals("name") || part.equals("gen-pattern");
        if (pattern && assign) sb.append("-alias");
        if (!pattern && lhs) sb.append("-match");
      }
      if (data != null) sb.append(" in ").append(data);
      return sb.toString();
    }
    return "File";
  }

  /**
   * What the plugin thinks of the variable at the offset, in parentheses, for the signature of a warning about it; empty
   * for other warnings. {@code extra}: the plugin warns and erlc does not.
   * <ul>
   *   <li>unused variable that only erlc reports: what the variable resolves to, else its first use, else {@code no use};</li>
   *   <li>unused variable that only the plugin reports: what the next occurrence of the name resolves to;</li>
   *   <li>unbound variable that only the plugin reports: the first occurrence of the name in the clause;</li>
   *   <li>unbound variable that only erlc reports: what the variable resolves to.</li>
   * </ul>
   */
  static String relation(PsiFile file, int offset, String kind, boolean extra) {
    if (!kind.endsWith("_var")) return "";
    ErlangQVar v = PsiTreeUtil.getParentOfType(file.findElementAt(offset), ErlangQVar.class, false);
    if (v == null) return "";
    PsiElement scope = PsiTreeUtil.getParentOfType(v, ErlangFunctionClause.class);
    if (scope == null) scope = file;
    List<ErlangQVar> same = new ArrayList<>();
    for (ErlangQVar q : SyntaxTraverser.psiTraverser(scope).filter(ErlangQVar.class)) if (q.getText().equals(v.getText())) same.add(q);
    int at = same.indexOf(v);
    PsiElement target = v.getReference().resolve();
    boolean resolves = target != null && target != v;
    if ("unused_var".equals(kind) && !extra) {
      if (resolves) return " (resolves to " + place(target) + ")";
      for (PsiReference ref : ReferencesSearch.search(v, new LocalSearchScope(scope)).findAll()) {
        if (ref.getElement() != v) return " (used at " + place(ref.getElement()) + ")";
      }
      return " (no use)";
    }
    if ("unused_var".equals(kind)) {
      if (at < 0 || at + 1 >= same.size()) return " (no next)";
      ErlangQVar next = same.get(at + 1);
      PsiElement t = next.getReference().resolve();
      if (t == null || t == next) return " (next unresolved)";
      return t == v ? " (next resolves to it)" : " (next resolves to " + place(t) + ")";
    }
    if (extra) return at > 0 ? " (first at " + place(same.get(0)) + ")" : " (no earlier)";
    return resolves ? " (resolves to " + place(target) + ")" : " (unresolved)";
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
    // erl_parse: "head mismatch: function f1 with arities 1 and 0 is regarded as two distinct functions. ... in f1/1 ..."
    return sb.toString().replaceAll("function \\S+ with", "function <atom> with").replaceAll("\\b[a-z]\\w*/\\d+", "<atom>/<number>")
      .replaceAll("arities \\d+ and \\d+", "arities <number> and <number>");
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
    for (Path f : erlFiles(Paths.get("testData/pbt/controls"))) {
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

  /** The file itself, or the {@code .erl} files of the directory, sorted. */
  private static List<Path> erlFiles(Path path) throws IOException {
    if (!Files.isDirectory(path)) return List.of(path);
    List<Path> files = new ArrayList<>();
    try (var stream = Files.list(path)) {
      stream.filter(p -> p.toString().endsWith(".erl")).sorted().forEach(files::add);
    }
    return files;
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
