package org.intellij.erlang.parser.spike;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

/**
 * Spike harness (macro substitution experiment). It is the same check as the triage desk's ErlangCorpusParsingTest,
 * extended to several corpora per run and to a speed run. It only uses the public plugin API, so the same file runs
 * in the unmodified baseline export and in the spike branch.
 * <p>
 * SPIKE_CORPORA = "name=dir;name=dir", SPIKE_REPORT_DIR = output directory (one name.json per corpus).
 * SPIKE_SPEED_OUT = file: when set, testSpeed parses every corpus file SPIKE_SPEED_REPS times (default 5, after one
 * warm-up pass) and writes the per-pass times, and the times of the five largest files.
 * No-op when SPIKE_CORPORA is not set.
 */
public class ErlangSpikeCorpusTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  static String env(String name) {
    String v = System.getProperty(name);
    if (v == null || v.isEmpty()) v = System.getenv(name);
    return v == null || v.isEmpty() ? null : v;
  }

  public static LinkedHashMap<String, Path> corpora() throws IOException {
    LinkedHashMap<String, Path> result = new LinkedHashMap<>();
    String spec = env("SPIKE_CORPORA");
    if (spec == null) return result;
    for (String part : spec.split(";")) {
      int eq = part.indexOf('=');
      result.put(part.substring(0, eq), Paths.get(part.substring(eq + 1)).toRealPath());
    }
    return result;
  }

  public static List<Path> files(Path root) throws IOException {
    try (Stream<Path> s = Files.walk(root)) {
      return s.filter(p -> {
        String n = p.getFileName().toString();
        return Files.isRegularFile(p) && (n.endsWith(".erl") || n.endsWith(".hrl"));
      }).sorted().toList();
    }
  }

  public static String read(Path file) throws IOException {
    byte[] bytes = Files.readAllBytes(file);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  public void testCorpus() throws Exception {
    LinkedHashMap<String, Path> corpora = corpora();
    String outDir = env("SPIKE_REPORT_DIR");
    if (corpora.isEmpty() || outDir == null) return;
    for (Map.Entry<String, Path> corpus : corpora.entrySet()) {
      runCorpus(corpus.getKey(), corpus.getValue(), Paths.get(outDir).resolve(corpus.getKey() + ".json"));
    }
  }

  private void runCorpus(String corpusName, Path root, Path out) throws IOException {
    List<Path> files = files(root);
    StringBuilder errorsJson = new StringBuilder();
    StringBuilder problemsJson = new StringBuilder();
    int filesWithErrors = 0;
    int totalErrors = 0;
    long start = System.currentTimeMillis();
    for (Path file : files) {
      String rel = root.relativize(file).toString();
      String text;
      try {
        text = read(file);
      }
      catch (IOException e) {
        appendProblem(problemsJson, rel, "io", e.toString(), 0);
        continue;
      }
      String name = file.getFileName().toString();
      long t0 = System.currentTimeMillis();
      List<String> errors;
      try {
        errors = collectErrors(name, text, rel);
      }
      catch (Throwable c) {
        appendProblem(problemsJson, rel, "exception", c + " at " + (c.getStackTrace().length > 0 ? c.getStackTrace()[0] : "?"),
                      System.currentTimeMillis() - t0);
        continue;
      }
      if (!errors.isEmpty()) {
        filesWithErrors++;
        totalErrors += errors.size();
        for (String err : errors) {
          if (!errorsJson.isEmpty()) errorsJson.append(",\n");
          errorsJson.append("    ").append(err);
        }
      }
    }
    long totalMs = System.currentTimeMillis() - start;
    String json = "{\n" +
                  "  \"corpus\": " + q(root.toString()) + ",\n" +
                  "  \"files\": " + files.size() + ",\n" +
                  "  \"filesWithErrors\": " + filesWithErrors + ",\n" +
                  "  \"errors\": " + totalErrors + ",\n" +
                  "  \"totalMs\": " + totalMs + ",\n" +
                  "  \"problems\": [\n" + problemsJson + "\n  ],\n" +
                  "  \"errorElements\": [\n" + errorsJson + "\n  ]\n" +
                  "}\n";
    if (out.getParent() != null) Files.createDirectories(out.getParent());
    Files.writeString(out, json);
    System.out.println("CORPUS " + corpusName + ": files=" + files.size() + " filesWithErrors=" + filesWithErrors +
                       " errors=" + totalErrors + " totalMs=" + totalMs + " report=" + out.toAbsolutePath());
  }

  private List<String> collectErrors(String name, String text, String rel) {
    return ReadAction.compute(() -> {
      PsiFile psi = PsiFileFactory.getInstance(getProject()).createFileFromText(name, ErlangLanguage.INSTANCE, text);
      Collection<PsiErrorElement> errors = PsiTreeUtil.collectElementsOfType(psi, PsiErrorElement.class);
      List<String> result = new ArrayList<>();
      for (PsiErrorElement e : errors) {
        int offset = e.getTextRange().getStartOffset();
        int line = 0;
        int lineStart = 0;
        for (int i = 0; i < offset && i < text.length(); i++) {
          if (text.charAt(i) == '\n') {
            line++;
            lineStart = i + 1;
          }
        }
        int lineEnd = text.indexOf('\n', lineStart);
        String sourceLine = text.substring(lineStart, lineEnd < 0 ? text.length() : lineEnd);
        result.add("{\"file\": " + q(rel) +
                   ", \"line\": " + (line + 1) +
                   ", \"column\": " + (offset - lineStart + 1) +
                   ", \"description\": " + q(e.getErrorDescription()) +
                   ", \"sourceLine\": " + q(sourceLine) + "}");
      }
      return result;
    });
  }

  /** Lex+parse time of the whole corpus, and of the five largest files. */
  public void testSpeed() throws Exception {
    String outPath = env("SPIKE_SPEED_OUT");
    LinkedHashMap<String, Path> corpora = corpora();
    if (outPath == null || corpora.isEmpty()) return;
    int reps = env("SPIKE_SPEED_REPS") == null ? 5 : Integer.parseInt(env("SPIKE_SPEED_REPS"));
    List<String[]> texts = loadAll(corpora);
    List<String[]> largest = largest(texts, 5);
    StringBuilder out = new StringBuilder();
    out.append("label=").append(env("SPIKE_SPEED_LABEL")).append('\n');
    out.append("files=").append(texts.size()).append('\n');
    passAll(texts); // warm-up
    for (int rep = 0; rep < reps; rep++) {
      long t = passAll(texts);
      out.append("all rep=").append(rep).append(" ms=").append(t / 1_000_000).append('\n');
      for (String[] f : largest) {
        long best = Long.MAX_VALUE;
        long[] times = new long[5];
        for (int i = 0; i < times.length; i++) times[i] = parseOnce(f);
        Arrays.sort(times);
        out.append("large rep=").append(rep).append(" file=").append(f[0]).append(" chars=").append(f[2].length())
          .append(" medianUs=").append(times[times.length / 2] / 1000).append('\n');
      }
    }
    Path p = Paths.get(outPath);
    if (p.getParent() != null) Files.createDirectories(p.getParent());
    Files.writeString(p, out.toString());
    System.out.println(out);
  }

  public static List<String[]> loadAll(LinkedHashMap<String, Path> corpora) throws IOException {
    List<String[]> texts = new ArrayList<>();
    for (Map.Entry<String, Path> c : corpora.entrySet()) {
      for (Path f : files(c.getValue())) {
        texts.add(new String[]{c.getKey() + "/" + c.getValue().relativize(f), f.getFileName().toString(), read(f)});
      }
    }
    return texts;
  }

  public static List<String[]> largest(List<String[]> texts, int n) {
    List<String[]> sorted = new ArrayList<>(texts);
    sorted.sort(Comparator.comparingInt((String[] t) -> t[2].length()).reversed());
    return sorted.subList(0, Math.min(n, sorted.size()));
  }

  public long passAll(List<String[]> texts) {
    long total = 0;
    for (String[] t : texts) total += parseOnce(t);
    return total;
  }

  public long parseOnce(String[] t) {
    return ReadAction.compute(() -> {
      long t0 = System.nanoTime();
      PsiFile psi = PsiFileFactory.getInstance(getProject()).createFileFromText(t[1], ErlangLanguage.INSTANCE, t[2]);
      psi.getNode().getFirstChildNode();
      return System.nanoTime() - t0;
    });
  }

  private static void appendProblem(StringBuilder sb, String rel, String kind, String detail, long ms) {
    if (!sb.isEmpty()) sb.append(",\n");
    sb.append("    {\"file\": ").append(q(rel)).append(", \"kind\": ").append(q(kind))
      .append(", \"detail\": ").append(q(detail)).append(", \"ms\": ").append(ms).append("}");
  }

  public static String q(String s) {
    StringBuilder sb = new StringBuilder("\"");
    for (char c : s.toCharArray()) {
      switch (c) {
        case '"' -> sb.append("\\\"");
        case '\\' -> sb.append("\\\\");
        case '\n' -> sb.append("\\n");
        case '\r' -> sb.append("\\r");
        case '\t' -> sb.append("\\t");
        default -> {
          if (c < 0x20) sb.append(String.format("\\u%04x", (int)c));
          else sb.append(c);
        }
      }
    }
    return sb.append('"').toString();
  }
}
