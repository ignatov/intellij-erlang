package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.BufferedWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Spike 3 measurements. Each test is a no-op without its environment variables.
 * <ul>
 * <li>testCounters: SPIKE_CORPORA (name=dir;...) and MEASURE_OUT (dir): form counters, copy build times, epp JSONL
 * of every expanded form, memory of 300 cached copies.</li>
 * <li>testSeeds: SEED_FILES (one path per line) and SEED_OUT (dir): seeded errors, highlighted off and layer.</li>
 * <li>testHighlightTime: TIME_FILE (path) and MEASURE_OUT: full highlighting time, off and layer, interleaved.</li>
 * </ul>
 */
public class ErlangMacroLayerMeasureTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  private void enableAllInspections() {
    List<com.intellij.codeInspection.InspectionProfileEntry> tools = new ArrayList<>();
    for (com.intellij.codeInspection.LocalInspectionEP ep : com.intellij.codeInspection.LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new com.intellij.codeInspection.InspectionProfileEntry[0]));
  }

  static String env(String name) {
    String v = System.getenv(name);
    if (v == null || v.isEmpty()) v = System.getProperty(name);
    return v == null || v.isEmpty() ? null : v;
  }

  static String read(Path file) throws Exception {
    byte[] bytes = Files.readAllBytes(file);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  static String q(String s) {
    StringBuilder sb = new StringBuilder("\"");
    for (char c : s.toCharArray()) {
      if (c == '"' || c == '\\') sb.append('\\').append(c);
      else if (c == '\n') sb.append("\\n");
      else if (c == '\t') sb.append("\\t");
      else if (c < 0x20) sb.append(String.format("\\u%04x", (int)c));
      else sb.append(c);
    }
    return sb.append('"').toString();
  }

  private static int line(String text, int offset) {
    int l = 1;
    for (int i = 0; i < offset && i < text.length(); i++) if (text.charAt(i) == '\n') l++;
    return l;
  }

  private static String category(IElementType type) {
    if (type == ErlangTypes.ERL_ATOM_NAME) return "atom";
    if (type == ErlangTypes.ERL_VAR) return "var";
    if (type == ErlangTypes.ERL_INTEGER) return "integer";
    if (type == ErlangTypes.ERL_FLOAT) return "float";
    if (type == ErlangTypes.ERL_CHAR) return "char";
    if (type == ErlangTypes.ERL_STRING || type == ErlangTypes.ERL_TRIPLE_QUOTED_STRING) return "string";
    if (type == ErlangTypes.ERL_SIGIL_STRING) return "sigil";
    if (type == ErlangTypes.ERL_SINGLE_QUOTE) return "quote";
    return "sym";
  }

  // ---------------------------------------------------------------------------------------------- counters, epp, memory

  public void testCounters() throws Exception {
    String spec = env("SPIKE_CORPORA");
    String outDir = env("MEASURE_OUT");
    if (spec == null || outDir == null) return;
    Path out = Paths.get(outDir);
    Files.createDirectories(out);
    EnumMap<FormStatus, Integer> forms = new EnumMap<>(FormStatus.class);
    EnumMap<ErlangMacroExpander.Outcome, Integer> uses = new EnumMap<>(ErlangMacroExpander.Outcome.class);
    int brokenExpanded = 0, expandedFiles = 0, files = 0, fragmentForms = 0, fragmentBroken = 0;
    List<long[]> buildTimes = new ArrayList<>(); // text length, nanos, file index
    List<String> names = new ArrayList<>();
    List<Object> held = new ArrayList<>();
    long heapBefore = 0, heapAfter = 0;
    int heldFiles = 0;
    StringBuilder overCap = new StringBuilder();
    try (BufferedWriter epp = Files.newBufferedWriter(out.resolve("forms-layer.jsonl"))) {
      for (String part : spec.split(";")) {
        String corpus = part.substring(0, part.indexOf('='));
        Path root = Paths.get(part.substring(part.indexOf('=') + 1)).toRealPath();
        List<Path> list;
        try (Stream<Path> s = Files.walk(root)) {
          list = s.filter(p -> Files.isRegularFile(p) && p.toString().endsWith(".erl")).sorted().toList();
        }
        for (Path p : list) {
          String text;
          try {
            text = read(p);
          }
          catch (Exception e) {
            continue;
          }
          files++;
          String rel = root.relativize(p).toString();
          if (heldFiles == 0 && files == 1) {
            System.gc();
            heapBefore = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
          }
          final String t = text;
          ErlangMacroLayer.Copy copy = ReadAction.compute(() -> {
            PsiFile psi = PsiFileFactory.getInstance(getProject()).createFileFromText(p.getFileName().toString(), ErlangLanguage.INSTANCE, t);
            return ErlangMacroLayer.build(psi);
          });
          Expansion x = copy.expansion;
          for (Map.Entry<ErlangMacroExpander.Outcome, Integer> e : x.outcomes.entrySet()) uses.merge(e.getKey(), e.getValue(), Integer::sum);
          boolean any = false;
          for (Form f : x.forms) {
            forms.merge(f.status, 1, Integer::sum);
            if (f.status == FormStatus.OVER_CAP) overCap.append(corpus).append(' ').append(rel).append(':').append(line(text, f.srcStart))
              .append(" tokens=").append(f.expandedTokens).append('\n');
            if (f.status != FormStatus.EXPANDED) continue;
            any = true;
            boolean broken = copy.isBroken(f);
            if (broken) brokenExpanded++;
            if (f.allFragments) {
              fragmentForms++;
              if (broken) fragmentBroken++;
            }
            // epp JSONL: the tokens of the expanded form
            ErlangLexer lexer = new ErlangLexer(getProject());
            lexer.start(x.text, f.expStart, f.expEnd, 0);
            List<String> toks = new ArrayList<>();
            for (IElementType tt; (tt = lexer.getTokenType()) != null; lexer.advance()) {
              if (tt == TokenType.WHITE_SPACE || ErlangParserDefinition.COMMENTS.contains(tt)) continue;
              toks.add("[" + q(category(tt)) + "," + q(x.text.substring(lexer.getTokenStart(), lexer.getTokenEnd())) + "]");
            }
            epp.write("{\"corpus\":" + q(corpus) + ",\"file\":" + q(rel) + ",\"line\":" + line(text, f.srcStart) +
                      ",\"fragments\":" + f.allFragments + ",\"broken\":" + broken + ",\"tokens\":[" + String.join(",", toks) + "]}\n");
          }
          if (any) expandedFiles++;
          buildTimes.add(new long[]{text.length(), copy.buildNanos, names.size()});
          names.add(corpus + " " + rel);
          if (any && heldFiles < 300) {
            held.add(copy);
            heldFiles++;
            if (heldFiles == 300) {
              for (int i = 0; i < 3; i++) System.gc();
              heapAfter = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
            }
          }
        }
      }
    }
    // the five largest files: build time again, 3 times, best
    buildTimes.sort((a, b) -> Long.compare(b[0], a[0]));
    StringBuilder sb = new StringBuilder();
    sb.append("files=").append(files).append(" filesWithExpandedForms=").append(expandedFiles).append('\n');
    sb.append("forms by status: ").append(forms).append('\n');
    sb.append("expanded forms whose copy has parse errors: ").append(brokenExpanded).append('\n');
    sb.append("fragment-only expanded forms: ").append(fragmentForms).append(" of them broken: ").append(fragmentBroken).append('\n');
    sb.append("uses by outcome: ").append(uses).append('\n');
    sb.append("held copies: ").append(heldFiles).append(" heap delta MB: ").append((heapAfter - heapBefore) / 1e6).append(" (held list size ").append(held.size()).append(")\n");
    sb.append("five largest files, copy build ms (first build in the run):\n");
    for (int i = 0; i < Math.min(5, buildTimes.size()); i++) {
      long[] b = buildTimes.get(i);
      sb.append("  ").append(names.get((int)b[2])).append(" chars=").append(b[0]).append(" ms=").append(b[1] / 1_000_000.0).append('\n');
    }
    long total = 0;
    for (long[] b : buildTimes) total += b[1];
    sb.append("total build ms for all files: ").append(total / 1_000_000).append('\n');
    Files.writeString(out.resolve("counters.txt"), sb.toString());
    Files.writeString(out.resolve("over-cap.txt"), overCap.toString());
    System.out.println(sb);
  }

  // ---------------------------------------------------------------------------------------------- seeded errors (R5)

  private static final Pattern FUNCTION_FORM = Pattern.compile("^([a-z][A-Za-z0-9_@]*)\\(");

  public void testSeeds() throws Exception {
    String list = env("SEED_FILES");
    String outDir = env("SEED_OUT");
    if (list == null || outDir == null) return;
    enableAllInspections();
    Path out = Paths.get(outDir);
    Files.createDirectories(out.resolve("texts"));
    String[] kinds = {"syntax", "unbound", "undefined", "arity"};
    int n = 0;
    try (BufferedWriter rows = Files.newBufferedWriter(out.resolve("seeds.jsonl"))) {
      for (String path : Files.readAllLines(Paths.get(list))) {
        if (path.isBlank()) continue;
        Path p = Paths.get(path.trim());
        String text = read(p);
        Expansion x = ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), ErlangMacroLayer.FORM_CAP);
        Form chosen = null;
        int at = -1;
        String fname = null;
        for (Form f : x.forms) {
          if (f.status != FormStatus.EXPANDED) continue;
          Matcher m = FUNCTION_FORM.matcher(text.substring(f.srcStart, Math.min(f.srcEnd, f.srcStart + 200)));
          if (!m.find()) continue;
          int arrow = text.indexOf("->", f.srcStart);
          while (arrow >= 0 && arrow < f.srcEnd && x.toExpanded(arrow) < 0) arrow = text.indexOf("->", arrow + 2);
          if (arrow < 0 || arrow >= f.srcEnd) continue;
          chosen = f;
          at = arrow + 2;
          fname = m.group(1);
          break;
        }
        if (chosen == null) {
          rows.write("{\"file\":" + q(path) + ",\"skipped\":\"no expanded function form with a verbatim ->\"}\n");
          continue;
        }
        for (String kind : kinds) {
          String insert = switch (kind) {
            case "syntax" -> " ]] ";
            case "unbound" -> " ZzSeedUnbound, ";
            case "undefined" -> " zz_seed_undefined_fn(), ";
            default -> " " + fname + "(1, 2, 3, 4, 5, 6, 7, 8, 9), ";
          };
          String seeded = text.substring(0, at) + insert + text.substring(at);
          int s0 = at, s1 = at + insert.length();
          String name = "s" + (n++) + "_" + p.getFileName();
          Files.writeString(out.resolve("texts").resolve(name), seeded);
          Map<String, String> found = new LinkedHashMap<>();
          for (ErlangMacroLayer.Mode mode : new ErlangMacroLayer.Mode[]{ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER}) {
            com.intellij.openapi.Disposable d = Disposer.newDisposable();
            ErlangMacroLayer.setModeForTests(mode, d);
            try {
              myFixture.configureByText(mode + "_" + name, seeded);
              List<HighlightInfo> infos = myFixture.doHighlighting();
              StringBuilder hit = new StringBuilder();
              for (HighlightInfo h : infos) {
                if (h.getSeverity().compareTo(HighlightSeverity.WEAK_WARNING) < 0) continue;
                if (h.getEndOffset() < s0 - 1 || h.getStartOffset() > s1 + 1) continue;
                hit.append(h.getSeverity()).append(": ").append(h.getDescription()).append("; ");
              }
              found.put(mode.name(), hit.toString());
            }
            finally {
              Disposer.dispose(d);
            }
          }
          rows.write("{\"file\":" + q(path) + ",\"seedFile\":" + q(name) + ",\"kind\":" + q(kind) + ",\"line\":" + line(seeded, at) +
                     ",\"off\":" + q(found.get("OFF")) + ",\"layer\":" + q(found.get("LAYER")) + "}\n");
          rows.flush();
        }
      }
    }
  }

  // ---------------------------------------------------------------------------------------------- highlighting time (R4)

  public void testHighlightTime() throws Exception {
    String file = env("TIME_FILE");
    String outDir = env("MEASURE_OUT");
    if (file == null || outDir == null) return;
    enableAllInspections();
    String text = read(Paths.get(file));
    StringBuilder sb = new StringBuilder();
    int n = 0;
    for (int rep = 0; rep < 3; rep++) {
      for (ErlangMacroLayer.Mode mode : rep % 2 == 0 ? new ErlangMacroLayer.Mode[]{ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER}
                                                     : new ErlangMacroLayer.Mode[]{ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.Mode.OFF}) {
        com.intellij.openapi.Disposable d = Disposer.newDisposable();
        ErlangMacroLayer.setModeForTests(mode, d);
        try {
          myFixture.configureByText("h" + (n++) + "_" + Paths.get(file).getFileName(), text);
          int built = ErlangMacroLayer.EXPANSIONS_BUILT.get();
          long t0 = System.nanoTime();
          List<HighlightInfo> infos = myFixture.doHighlighting();
          long ms = (System.nanoTime() - t0) / 1_000_000;
          sb.append("rep ").append(rep).append(' ').append(mode).append(": ").append(ms).append(" ms, infos=").append(infos.size())
            .append(", copies built=").append(ErlangMacroLayer.EXPANSIONS_BUILT.get() - built).append('\n');
        }
        finally {
          Disposer.dispose(d);
        }
      }
    }
    Files.createDirectories(Paths.get(outDir));
    Files.writeString(Paths.get(outDir, "time-" + Paths.get(file).getFileName() + ".txt"), sb.toString());
    System.out.println(sb);
  }
}
