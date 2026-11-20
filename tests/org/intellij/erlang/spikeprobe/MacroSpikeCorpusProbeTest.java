package org.intellij.erlang.spikeprobe;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.ex.FileEditorManagerEx;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubTextInconsistencyException;
import com.intellij.psi.stubs.StubTree;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.indexing.ID;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/**
 * Prober of the macro spike: runs reformat, full highlighting, stub and atom index reads and edits near macro uses
 * over a list of files and writes what it sees. It never fails on a finding. No-op without PROBE_FILES and PROBE_OUT.
 * PROBE_FILES: lines "id TAB absolute path". PROBE_OPS: subset of tree,stubs,atoms,highlight,format,reparse.
 */
public class MacroSpikeCorpusProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final Gson GSON = new GsonBuilder().disableHtmlEscaping().create();
  private static final Pattern USE = Pattern.compile("\\?'?([A-Za-z_][A-Za-z0-9_@]*)'?");

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testCorpus() throws Exception {
    String list = env("PROBE_FILES");
    String out = env("PROBE_OUT");
    if (list == null || out == null) return;
    Path dir = Paths.get(out, label());
    Files.createDirectories(dir.resolve("format"));
    Files.createDirectories(dir.resolve("stubs"));
    Set<String> ops = new HashSet<>(Arrays.asList(
      Objects.requireNonNullElse(env("PROBE_OPS"), "tree,stubs,atoms,highlight,format,reparse").split(",")));
    if (ops.contains("highlight")) enableAllInspections();
    Path results = dir.resolve("corpus.jsonl");
    Set<String> done = new HashSet<>();
    if (env("PROBE_RESUME") != null && Files.exists(results)) {
      for (String l : Files.readAllLines(results)) {
        if (l.isBlank()) continue;
        done.add(com.google.gson.JsonParser.parseString(l).getAsJsonObject().get("id").getAsString());
      }
    }
    else {
      Files.deleteIfExists(results);
    }
    int budget = env("PROBE_BUDGET_SEC") == null ? 240 : Integer.parseInt(env("PROBE_BUDGET_SEC"));
    Thread watchdog = new Thread(() -> {
      while (true) {
        try {
          Thread.sleep(2000);
        }
        catch (InterruptedException e) {
          return;
        }
        String id = myCurrentId;
        long start = myCurrentStart;
        if (id != null && System.currentTimeMillis() - start > budget * 1000L) {
          JsonObject t = new JsonObject();
          t.addProperty("id", id);
          t.addProperty("timeout", true);
          t.addProperty("op", myCurrentOp);
          t.addProperty("seconds", (System.currentTimeMillis() - start) / 1000);
          try {
            Files.writeString(results, GSON.toJson(t) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND);
          }
          catch (Exception ignored) {
          }
          System.err.println("PROBE: timeout in " + id + " " + myCurrentOp + ", halting");
          Runtime.getRuntime().halt(3);
        }
      }
    }, "probe-watchdog");
    watchdog.setDaemon(true);
    watchdog.start();
    for (String line : Files.readAllLines(Paths.get(list))) {
      if (line.isBlank()) continue;
      String[] parts = line.split("\t");
      String id = parts[0];
      if (done.contains(id)) continue;
      Path path = Paths.get(parts[1]);
      myCurrentStart = System.currentTimeMillis();
      myCurrentId = id;
      JsonObject r = probe(id, path, ops, dir);
      myCurrentId = null;
      Files.writeString(results, GSON.toJson(r) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND);
    }
    watchdog.interrupt();
  }

  private volatile String myCurrentId;
  private volatile long myCurrentStart;
  private volatile String myCurrentOp;

  private long op(JsonObject r, String name, long t0) {
    long now = System.currentTimeMillis();
    if (t0 > 0) r.addProperty(myCurrentOp + "Ms", now - t0);
    myCurrentOp = name;
    return now;
  }

  private void enableAllInspections() {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  private JsonObject probe(String id, Path path, Set<String> ops, Path dir) throws Exception {
    JsonObject r = new JsonObject();
    r.addProperty("id", id);
    r.addProperty("path", path.toString());
    String text = read(path);
    VirtualFile[] vf = new VirtualFile[1];
    List<Failure> setup = guarded(() -> {
      VirtualFile f = myFixture.getTempDirFixture().createFile("p" + id + "/" + path.getFileName(), text);
      vf[0] = f;
      myFixture.configureFromExistingVirtualFile(f);
    });
    if (!setup.isEmpty() || vf[0] == null) {
      r.add("setup", failures(setup));
      return r;
    }
    Document doc = myFixture.getEditor().getDocument();
    String original = doc.getText();
    long t = op(r, "setupDone", 0);
    if (ops.contains("tree")) {
      int[] c = treeCounts(myFixture.getFile());
      r.addProperty("errors", c[0]);
      r.addProperty("foreign", c[1]);
      r.addProperty("calls", c[2]);
    }
    t = op(r, "stubs", t);
    if (ops.contains("stubs")) {
      String[] stub = new String[1];
      List<Failure> f = guarded(() -> {
        PsiFileImpl psi = (PsiFileImpl)myFixture.getFile();
        StubTree tree = psi.calcStubTree();
        stub[0] = DebugUtil.stubTreeToString(tree.getRoot());
        StubTextInconsistencyException.checkStubTextConsistency(psi);
      });
      if (stub[0] != null) Files.writeString(dir.resolve("stubs").resolve(id + ".txt"), stub[0]);
      r.add("stubsFailures", failures(f));
    }
    t = op(r, "atoms", t);
    if (ops.contains("atoms")) {
      List<String> keys = new ArrayList<>();
      List<Failure> f = guarded(() -> {
        ID<String, Void> atomIndex = ID.findByName("erlang.atom.index");
        Map<String, Void> data = FileBasedIndex.getInstance().getFileData(atomIndex, vf[0], getProject());
        keys.addAll(data.keySet());
      });
      Collections.sort(keys);
      JsonArray a = new JsonArray();
      for (String k : keys) a.add(k);
      r.add("atoms", a);
      r.add("atomsFailures", failures(f));
    }
    t = op(r, "highlight", t);
    if (ops.contains("highlight")) {
      JsonArray infos = new JsonArray();
      List<Failure> f = guarded(() -> {
        for (HighlightInfo info : myFixture.doHighlighting()) {
          if (info.getDescription() == null) continue;
          JsonObject o = new JsonObject();
          o.addProperty("sev", info.getSeverity().getName());
          o.addProperty("tool", info.getInspectionToolId());
          o.addProperty("desc", clip(info.getDescription(), 200));
          o.addProperty("start", info.getStartOffset());
          o.addProperty("end", info.getEndOffset());
          infos.add(o);
        }
      });
      r.add("highlight", infos);
      r.add("highlightFailures", failures(f));
    }
    t = op(r, "format", t);
    if (ops.contains("format")) {
      restore(original);
      String[] texts = new String[2];
      List<Failure> f = guarded(() -> {
        PsiDocumentManager pdm = PsiDocumentManager.getInstance(getProject());
        WriteCommandAction.runWriteCommandAction(getProject(), () -> { CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile()); });
        pdm.commitAllDocuments();
        texts[0] = doc.getText();
        WriteCommandAction.runWriteCommandAction(getProject(), () -> { CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile()); });
        pdm.commitAllDocuments();
        texts[1] = doc.getText();
      });
      if (texts[0] != null) Files.writeString(dir.resolve("format").resolve(id + ".erl"), texts[0]);
      r.addProperty("formatChanged", texts[0] != null && !texts[0].equals(original));
      r.addProperty("formatStable", texts[0] != null && texts[0].equals(texts[1]));
      r.addProperty("formatNonWsChanged", texts[0] != null && !stripWs(texts[0]).equals(stripWs(original)));
      r.add("formatFailures", failures(f));
    }
    t = op(r, "reparse", t);
    if (ops.contains("reparse")) {
      JsonArray edits = new JsonArray();
      for (int[] edit : edits(original)) {
        restore(original);
        JsonObject e = new JsonObject();
        e.addProperty("kind", edit[0]);
        e.addProperty("offset", edit[1]);
        e.addProperty("len", edit[2]);
        String insert = edit[0] == 4 ? "" : edit[0] == 2 ? "x" : " ";
        List<Failure> f = guarded(() -> {
          WriteCommandAction.runWriteCommandAction(getProject(), () -> {
            if (edit[2] > 0) doc.deleteString(edit[1], edit[1] + edit[2]);
            if (!insert.isEmpty()) doc.insertString(edit[1], insert);
          });
          PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
          PsiFile file = myFixture.getFile();
          if (!file.getText().equals(doc.getText())) throw new AssertionError("PSI text differs from the document");
          PsiTestUtil.checkFileStructure(file);
        });
        e.add("failures", failures(f));
        edits.add(e);
      }
      restore(original);
      r.add("reparse", edits);
    }
    op(r, "cleanup", t);
    List<Failure> cleanup = guarded(() -> {
      FileEditorManagerEx.getInstanceEx(getProject()).closeAllFiles();
      WriteAction.run(() -> vf[0].delete(this));
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    });
    if (!cleanup.isEmpty()) r.add("cleanup", failures(cleanup));
    return r;
  }

  /**
   * Edits near the first three uses of macros defined in the file: kind 1 a space before the use, 2 an 'x' inside the
   * name, 3 a space after the use, 4 the deletion of the -define, 5 a space inside the body of the -define.
   */
  static List<int[]> edits(String text) {
    List<int[]> result = new ArrayList<>();
    Matcher m = USE.matcher(text);
    int uses = 0;
    while (m.find() && uses < 3) {
      String name = m.group(1);
      int def = text.indexOf("-define(" + name);
      if (def < 0 || def > m.start()) continue;
      if (m.start() > 0 && text.charAt(m.start() - 1) == '?') continue;
      if (inComment(text, m.start())) continue;
      int end = m.end();
      if (end < text.length() && text.charAt(end) == '(') {
        int depth = 0;
        for (int i = end; i < text.length(); i++) {
          char c = text.charAt(i);
          if (c == '(') depth++;
          else if (c == ')' && --depth == 0) {
            end = i + 1;
            break;
          }
        }
      }
      result.add(new int[]{1, m.start(), 0});
      result.add(new int[]{2, m.start() + 2, 0});
      result.add(new int[]{3, end, 0});
      if (uses == 0) {
        int defEnd = text.indexOf(").\n", def);
        if (defEnd > 0) {
          result.add(new int[]{4, def, defEnd + 2 - def});
          int comma = text.indexOf(',', def);
          if (comma > 0 && comma < defEnd) result.add(new int[]{5, comma + 1, 0});
        }
      }
      uses++;
    }
    return result;
  }

  static boolean inComment(String text, int offset) {
    int lineStart = text.lastIndexOf('\n', offset - 1) + 1;
    boolean inString = false;
    for (int i = lineStart; i < offset; i++) {
      char c = text.charAt(i);
      if (c == '"') inString = !inString;
      if (c == '%' && !inString) return true;
    }
    return inString;
  }

  private void restore(String original) {
    Document doc = myFixture.getEditor().getDocument();
    if (!doc.getText().equals(original)) {
      WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.setText(original));
    }
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  static String stripWs(String s) {
    StringBuilder sb = new StringBuilder(s.length());
    for (int i = 0; i < s.length(); i++) {
      char c = s.charAt(i);
      if (!Character.isWhitespace(c)) sb.append(c);
    }
    return sb.toString();
  }

  static JsonArray failures(List<Failure> list) {
    JsonArray a = new JsonArray();
    for (Failure f : list) a.add(f.json());
    return a;
  }
}
