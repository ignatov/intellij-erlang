package org.intellij.erlang.regression;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.LocalTimeCounter;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.BufferedWriter;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.*;
import java.util.stream.Stream;

/**
 * The per-file probe of the regression check (scripts/regression/differential.sh): for every .erl and .hrl file under
 * the corpus it records the lexer token hash, the parse errors and the PSI tree hash, the formatter result, the problems
 * of every plugin inspection (also those that are off by default) and the timings, one JSON line per file. The script
 * runs it with the plugin built at two git refs and compares the two outputs with diffreport.py. It asserts nothing and
 * does nothing without -Ddiff.corpus; the {@code test} task excludes this package, {@code regressionDiff} runs it.
 * The script copies this file into the worktree of the other ref, so it must compile against the last release too.
 * <p>
 * -Ddiff.corpus=DIR -Ddiff.out=FILE.jsonl [-Ddiff.phases=lex,parse,format,highlight] [-Ddiff.only=LISTFILE]
 * [-Ddiff.dump=DIR] [-Ddiff.repeat=N] [-Ddiff.copy=true] [-Ddiff.shard=I/N] [-Ddiff.deadline=EPOCH_MILLIS]
 * [-Ddiff.retimeMs=MS] [-Ddiff.forkDirs=true] [-Ddiff.registry=KEY=VALUE,...]
 * <p>
 * After the deadline no further file is started; the last line of the output, {"end": true, ...}, says whether every
 * file was done.
 */
public class RegressionDiffTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static {
    // Parallel Gradle forks must not share the IDE system directory (the indices) of the test sandbox.
    String worker = System.getProperty("org.gradle.test.worker");
    if (worker != null && Boolean.getBoolean("diff.forkDirs")) {
      for (String key : new String[]{"idea.system.path", "idea.config.path"}) {
        String value = System.getProperty(key);
        if (value != null) System.setProperty(key, value + "-fork" + worker);
      }
    }
  }

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

  @Override
  protected boolean shouldRunTest() {
    return System.getProperty("diff.corpus") != null && super.shouldRunTest();
  }

  public void testRun() throws Exception {
    Path root = Paths.get(System.getProperty("diff.corpus")).toRealPath();
    String shardSpec = shardSpec();
    Path out = Paths.get(System.getProperty("diff.out") + (shardSpec != null && getClass() != RegressionDiffTest.class
                                                            ? "." + shardSpec.replace('/', '-') : "")).toAbsolutePath();
    Set<String> phases = new HashSet<>(Arrays.asList(System.getProperty("diff.phases", "lex,parse,format,highlight").split(",")));
    String onlyList = System.getProperty("diff.only");
    String dump = System.getProperty("diff.dump");
    int repeat = Integer.parseInt(System.getProperty("diff.repeat", "1"));
    long slowNs = Long.parseLong(System.getProperty("diff.retimeMs", "0")) * 1_000_000L;
    boolean copy = Boolean.parseBoolean(System.getProperty("diff.copy", "true"));
    int shard = shardSpec == null ? 0 : Integer.parseInt(shardSpec.split("/")[0]);
    int shards = shardSpec == null ? 1 : Integer.parseInt(shardSpec.split("/")[1]);
    long deadline = Long.parseLong(System.getProperty("diff.deadline", "0"));
    String registry = System.getProperty("diff.registry");
    if (registry != null) {
      for (String kv : registry.split(",")) {
        String[] p = kv.split("=");
        com.intellij.openapi.util.registry.RegistryValue rv = com.intellij.openapi.util.registry.Registry.get(p[0]);
        String before = rv.asString();
        rv.setValue(p[1]);
        com.intellij.openapi.util.Disposer.register(getTestRootDisposable(), () -> rv.setValue(before));
      }
    }
    Set<String> only = null;
    if (onlyList != null) {
      only = new LinkedHashSet<>();
      for (String l : Files.readAllLines(Paths.get(onlyList))) if (!l.isBlank()) only.add(l.trim());
    }

    List<Path> files;
    try (Stream<Path> walk = Files.walk(root)) {
      files = walk.filter(Files::isRegularFile)
        .filter(p -> p.toString().endsWith(".erl") || p.toString().endsWith(".hrl"))
        .sorted().toList();
    }
    if (shards > 1) {
      List<Path> mine = new ArrayList<>();
      for (int i = 0; i < files.size(); i++) if (i % shards == shard) mine.add(files.get(i));
      files = mine;
    }

    long t0 = System.nanoTime();
    if (phases.contains("highlight") && copy) {
      myFixture.getTempDirFixture().copyAll(root.toString(), "");
    }
    long copyMs = (System.nanoTime() - t0) / 1_000_000;

    List<InspectionProfileEntry> tools = new ArrayList<>();
    List<String> toolNames = new ArrayList<>();
    List<String> toolsOffByDefault = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass != null && ep.implementationClass.startsWith("org.intellij.erlang")) {
        tools.add(ep.instantiateTool());
        toolNames.add(ep.getShortName());
        if (!ep.enabledByDefault) toolsOffByDefault.add(ep.getShortName());
      }
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));

    // Warm-up: lex and parse everything once, untimed.
    for (Path file : files) {
      String rel = root.relativize(file).toString();
      if (only != null && !only.contains(rel)) continue;
      String text = StringUtil.convertLineSeparators(readText(file));
      lex(text);
      PsiFile f = createFile(rel, text, false);
      f.getNode().getFirstChildNode();
    }

    Files.createDirectories(out.getParent());
    try (BufferedWriter w = Files.newBufferedWriter(out, StandardCharsets.UTF_8)) {
      w.write("{\"meta\": true, \"copyMs\": " + copyMs + ", \"tools\": " + jsonList(toolNames) +
              ", \"toolsOffByDefault\": " + jsonList(toolsOffByDefault) + "}\n");
      int n = 0;
      int wanted = 0;
      boolean complete = true;
      for (Path file : files) {
        String rel = root.relativize(file).toString();
        if (only != null && !only.contains(rel)) continue;
        wanted++;
        if (deadline > 0 && System.currentTimeMillis() > deadline) {
          complete = false;
          continue;
        }
        n++;
        String text = StringUtil.convertLineSeparators(readText(file));
        StringBuilder rec = new StringBuilder("{\"file\": ").append(json(rel));
        rec.append(", \"lines\": ").append(StringUtil.countNewLines(text) + 1);
        List<String> exceptions = new ArrayList<>();

        if (phases.contains("lex")) {
          try {
            long best = Long.MAX_VALUE;
            String[] res = null;
            for (int i = 0; ; i++) {
              long s = System.nanoTime();
              res = lex(text);
              best = Math.min(best, System.nanoTime() - s);
              if (!again(i, repeat, best, slowNs)) break;
            }
            rec.append(", \"lexUs\": ").append(best / 1000).append(", \"tokens\": ").append(res[0])
              .append(", \"lexHash\": ").append(json(res[1]));
          }
          catch (Throwable e) { exceptions.add("lex: " + e); }
        }

        if (phases.contains("parse")) {
          try {
            long best = Long.MAX_VALUE;
            PsiFile f = null;
            for (int i = 0; ; i++) {
              long s = System.nanoTime();
              f = createFile(rel, text, false);
              f.getNode().getFirstChildNode();
              best = Math.min(best, System.nanoTime() - s);
              if (!again(i, repeat, best, slowNs)) break;
            }
            rec.append(", \"parseUs\": ").append(best / 1000);
            List<String> errs = new ArrayList<>();
            for (PsiErrorElement e : PsiTreeUtil.collectElementsOfType(f, PsiErrorElement.class)) {
              errs.add((StringUtil.offsetToLineColumn(text, e.getTextOffset()).line + 1) + ": " + e.getErrorDescription());
            }
            rec.append(", \"errors\": ").append(jsonList(errs));
            TreeMap<String, Integer> counts = new TreeMap<>();
            countTypes(f, counts);
            rec.append(", \"types\": {");
            boolean first = true;
            for (Map.Entry<String, Integer> en : counts.entrySet()) {
              if (!first) rec.append(", ");
              first = false;
              rec.append(json(en.getKey())).append(": ").append(en.getValue());
            }
            rec.append("}");
            String tree = DebugUtil.psiToString(f, true);
            rec.append(", \"treeHash\": ").append(json(sha(tree)));
            if (dump != null) write(Paths.get(dump, "tree", rel + ".txt"), tree);
          }
          catch (Throwable e) { exceptions.add("parse: " + e); }
        }

        if (phases.contains("format")) {
          try {
            PsiFile f = createFile(rel, text, true);
            long s = System.nanoTime();
            WriteCommandAction.runWriteCommandAction(getProject(), () -> {
              CodeStyleManager.getInstance(getProject()).reformat(f);
            });
            long us = (System.nanoTime() - s) / 1000;
            String formatted = f.getText();
            rec.append(", \"formatUs\": ").append(us).append(", \"formatChanged\": ").append(!formatted.equals(text))
              .append(", \"formatHash\": ").append(json(sha(formatted)))
              .append(", \"formatTokensChanged\": ").append(!codeTokens(text).equals(codeTokens(formatted)));
            if (dump != null) write(Paths.get(dump, "format", rel), formatted);
          }
          catch (Throwable e) { exceptions.add("format: " + e); }
        }

        if (phases.contains("highlight")) {
          try {
            VirtualFile vf = myFixture.findFileInTempDir(rel);
            if (vf == null) throw new IllegalStateException("not copied: " + rel);
            myFixture.configureFromExistingVirtualFile(vf);
            long best = Long.MAX_VALUE;
            List<HighlightInfo> infos = null;
            for (int i = 0; ; i++) {
              if (i > 0) myFixture.getPsiManager().dropPsiCaches();
              long s = System.nanoTime();
              infos = myFixture.doHighlighting();
              best = Math.min(best, System.nanoTime() - s);
              if (!again(i, repeat, best, slowNs)) break;
            }
            rec.append(", \"highlightUs\": ").append(best / 1000);
            List<String> problems = new ArrayList<>();
            TreeMap<String, Integer> infoKeys = new TreeMap<>();
            String docText = myFixture.getEditor().getDocument().getText();
            for (HighlightInfo info : infos) {
              HighlightSeverity sev = info.getSeverity();
              if (sev.compareTo(HighlightSeverity.WEAK_WARNING) >= 0 || sev == HighlightSeverity.WEAK_WARNING) {
                int line = StringUtil.offsetToLineColumn(docText, info.getStartOffset()).line + 1;
                problems.add(sev.getName() + "|" + info.getInspectionToolId() + "|" + line + "|" +
                             docText.substring(info.getStartOffset(), Math.min(info.getEndOffset(), info.getStartOffset() + 60)).replace('\n', ' ') +
                             "|" + info.getDescription());
              }
              else {
                TextAttributesKey key = info.forcedTextAttributesKey;
                String name = key != null ? key.getExternalName() : (info.type != null ? info.type.getAttributesKey().getExternalName() : "?");
                infoKeys.merge(name, 1, Integer::sum);
              }
            }
            Collections.sort(problems);
            rec.append(", \"problems\": ").append(jsonList(problems));
            rec.append(", \"infoKeys\": {");
            boolean first = true;
            for (Map.Entry<String, Integer> en : infoKeys.entrySet()) {
              if (!first) rec.append(", ");
              first = false;
              rec.append(json(en.getKey())).append(": ").append(en.getValue());
            }
            rec.append("}");
            FileEditorManager.getInstance(getProject()).closeFile(vf);
          }
          catch (Throwable e) { exceptions.add("highlight: " + e); }
        }
        rec.append(", \"exceptions\": ").append(jsonList(exceptions)).append("}\n");
        w.write(rec.toString());
        w.flush();
        if (n % 100 == 0) System.out.println("regression-diff: " + n + " files");
      }
      w.write("{\"end\": true, \"complete\": " + complete + ", \"files\": " + n + ", \"wanted\": " + wanted + "}\n");
    }
  }

  /**
   * Whether to time once more after run i (0-based): until diff.repeat runs are done, and then once more when the best
   * time so far is at least diff.retimeMs, so that a slow file is timed twice and keeps the faster time. One sample of
   * a slow file on a busy runner can be 2x off; formatter timings are never retimed.
   */
  private static boolean again(int i, int repeat, long bestNs, long slowNs) {
    return i + 1 < repeat || i + 1 == repeat && slowNs > 0 && bestNs >= slowNs;
  }

  /** "i/n": this run handles the files whose index modulo n is i. Subclasses fix it, so Gradle forks run shards. */
  protected String shardSpec() {
    return System.getProperty("diff.shard");
  }

  private static void countTypes(PsiElement e, Map<String, Integer> counts) {
    IElementType t = e.getNode().getElementType();
    counts.merge(t.toString(), 1, Integer::sum);
    for (PsiElement c = e.getFirstChild(); c != null; c = c.getNextSibling()) countTypes(c, counts);
  }

  private PsiFile createFile(String rel, String text, boolean events) {
    ErlangFileType type = rel.endsWith(".hrl") ? ErlangFileType.HEADER : ErlangFileType.MODULE;
    String name = Paths.get(rel).getFileName().toString();
    return PsiFileFactory.getInstance(getProject()).createFileFromText(name, type, text, LocalTimeCounter.currentTime(), events);
  }

  private String[] lex(String text) throws Exception {
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    MessageDigest md = MessageDigest.getInstance("SHA-1");
    int n = 0;
    while (lexer.getTokenType() != null) {
      md.update((lexer.getTokenType().toString() + "@" + lexer.getTokenStart() + ";").getBytes(StandardCharsets.UTF_8));
      n++;
      lexer.advance();
    }
    return new String[]{String.valueOf(n), HexFormat.of().formatHex(md.digest()).substring(0, 16)};
  }

  /** Tokens other than white space, as type and text: a formatter must never change them. */
  private List<String> codeTokens(String text) {
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    List<String> out = new ArrayList<>();
    while (lexer.getTokenType() != null) {
      if (lexer.getTokenType() != com.intellij.psi.TokenType.WHITE_SPACE) {
        out.add(lexer.getTokenType() + " " + lexer.getTokenText().strip());
      }
      lexer.advance();
    }
    return out;
  }

  private static String sha(String s) throws Exception {
    return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-1").digest(s.getBytes(StandardCharsets.UTF_8))).substring(0, 16);
  }

  private static void write(Path p, String s) throws Exception {
    Files.createDirectories(p.getParent());
    Files.writeString(p, s, StandardCharsets.UTF_8);
  }

  private static String readText(Path file) throws Exception {
    byte[] bytes = Files.readAllBytes(file);
    try {
      return StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString();
    }
    catch (CharacterCodingException e) {
      return new String(bytes, StandardCharsets.ISO_8859_1);
    }
  }

  private static String jsonList(List<String> items) {
    StringBuilder b = new StringBuilder("[");
    for (int i = 0; i < items.size(); i++) {
      if (i > 0) b.append(", ");
      b.append(json(items.get(i)));
    }
    return b.append("]").toString();
  }

  private static String json(String value) {
    if (value == null) return "null";
    StringBuilder result = new StringBuilder("\"");
    for (char c : value.toCharArray()) {
      switch (c) {
        case '"' -> result.append("\\\"");
        case '\\' -> result.append("\\\\");
        case '\n' -> result.append("\\n");
        case '\r' -> result.append("\\r");
        case '\t' -> result.append("\\t");
        default -> {
          if (c < 0x20) result.append(String.format("\\u%04x", (int) c));
          else result.append(c);
        }
      }
    }
    return result.append('"').toString();
  }
}
