package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.PlatformTestUtil;

import java.util.*;
import java.util.concurrent.Future;

/**
 * Typing states: random edit scripts (typed text, deletions, pasted macro uses, comment toggles, undo and redo, edits of
 * the other files of the project) with an editor action and a question to the plugin after each edit, asked at once
 * (the document not committed), after the commit, during highlighting and after it, from the UI thread and from a
 * background thread. Every answer must be a valid element; the final answers (references and highlighting) must equal
 * those of a fresh project with the same texts, and two fresh runs must agree.
 */
public class MacroFuzzTypingTest extends MacroFuzzTestBase {
  private static final String[] SNIPPETS = {"?", "(", ")", ",", ".", "\n", " ", "-define(", "?MODULE", "?C", "X", "->", ";", "end",
    "begin ", "[", "]", "|", "\"", "'", "%", "#", "{", "}", "?F(1)", "?F(", "-include(\"h1.hrl\").\n", "-undef(C).\n", "-ifdef(C).\n",
    "-endif.\n", "b:g()", "foo", "_", "=", "+", "??", "-define(C, 1).\n", "-define(F(A), A).\n", "?LISTS reverse(L)", ", X])"};

  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  public void testEditScriptsAgreeWithFreshRun() throws Throwable {
    EditorActionProbe.enableInspections(myFixture);
    runCases("typing", 3, (index, rnd, caseId) -> {
      Map<String, String> files = files(rnd);
      long scriptSeed = rnd.nextLong();
      int steps = smoke() ? 6 : 10 + rnd.nextInt(40);
      typingCase(files, scriptSeed, steps, new BitSet());
    });
  }

  private static Map<String, String> files(Random rnd) {
    Map<String, String> files = new LinkedHashMap<>(new MacroCaseGenerator(rnd).project());
    if (!files.containsKey("b.erl")) files.put("b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
    return files;
  }

  /**
   * The minimiser of an edit script: replays case {@code MACRO_FUZZ_MIN_TYPING_CASE} (with {@code MACRO_FUZZ_SEED}) with
   * steps left out (delta debugging) while a failure whose signature contains {@code MACRO_FUZZ_MIN_SIG} still appears.
   * Each step has its own random numbers, so leaving one out does not change the others. Prints the steps kept, the
   * starting texts and the final texts.
   */
  public void testMinimizeTypingCase() throws Throwable {
    String c = FuzzConfig.env("MACRO_FUZZ_MIN_TYPING_CASE");
    String sig = FuzzConfig.env("MACRO_FUZZ_MIN_SIG");
    if (c == null || sig == null) return;
    EditorActionProbe.enableInspections(myFixture);
    int index = Integer.parseInt(c);
    Random rnd = new Random(FuzzConfig.caseSeed(FuzzConfig.seed(), "typing", index));
    Map<String, String> files = files(rnd);
    long scriptSeed = rnd.nextLong();
    int steps = 10 + rnd.nextInt(40);
    List<Integer> kept = new ArrayList<>();
    for (int i = 0; i < steps; i++) kept.add(i);
    int[] trials = {0};
    java.util.function.Predicate<List<Integer>> fails = candidate -> {
      trials[0]++;
      BitSet skip = new BitSet();
      for (int i = 0; i < steps; i++) if (!candidate.contains(i)) skip.set(i);
      FuzzRecorder saved = rec;
      FuzzRecorder trial = new FuzzRecorder(getClass().getName() + "#trial", false);
      rec = trial;
      try {
        trial.runAll(() -> {
          trial.startCase("min:" + trials[0], 0, files);
          typingCase(files, scriptSeed, steps, skip);
        });
      }
      catch (Throwable t) {
        return false;
      }
      finally {
        rec = saved;
      }
      for (String s : trial.findings().keySet()) if (s.contains(sig)) return true;
      return false;
    };
    rec = new FuzzRecorder(getClass().getName() + "#" + getName());
    assertTrue("case " + index + " does not fail with '" + sig + "'", fails.test(kept));
    int n = 2;
    while (kept.size() >= 2 && trials[0] < 200) {
      int chunk = (int)Math.ceil(kept.size() / (double)n);
      boolean reduced = false;
      for (int st = 0; st < kept.size(); st += chunk) {
        List<Integer> complement = new ArrayList<>(kept.subList(0, st));
        complement.addAll(kept.subList(Math.min(kept.size(), st + chunk), kept.size()));
        if (!complement.isEmpty() && fails.test(complement)) {
          kept = complement;
          n = Math.max(n - 1, 2);
          reduced = true;
          break;
        }
      }
      if (!reduced) {
        if (n >= kept.size()) break;
        n = Math.min(kept.size(), n * 2);
      }
    }
    // one more run with the kept steps, reported with its trail and texts
    BitSet skip = new BitSet();
    for (int i = 0; i < steps; i++) if (!kept.contains(i)) skip.set(i);
    List<Integer> keptFinal = kept;
    rec.runAll(() -> {
      rec.startCase("typing-min:" + index, scriptSeed, files);
      typingCase(files, scriptSeed, steps, skip);
    });
    System.err.println("MACRO-FUZZ minimised script of typing:" + index + " after " + trials[0] + " trials: steps " + keptFinal +
                       "\nstarting texts: " + files + "\n" + rec.finish(0));
  }

  /** One edit script on a fresh world, then the comparison of its final answers with a fresh project. */
  private void typingCase(Map<String, String> files, long scriptSeed, int steps, BitSet skip) throws Throwable {
      FuzzWorld w = world(files, false);
      List<String> typedRefs;
      List<String> typedEdtRefs;
      List<String> typedHighlights;
      Map<String, String> finalTexts = new LinkedHashMap<>();
      String root = w.root;
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        warm(m);
        EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
        for (int step = 0; step < steps; step++) {
          if (skip.get(step)) continue;
          // every step has its own random numbers: leaving a step out does not change the others
          Random sr = new Random(scriptSeed + step * 0x9E3779B97F4A7C15L);
          String edit = edit(w, sr, probe);
          ask(w, sr, probe, "step" + step + "[" + edit + "] ");
        }
        // the final state: everything committed and highlighted
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        List<HighlightInfo>[] infos = new List[1];
        rec.guard("final highlight", () -> infos[0] = myFixture.doHighlighting());
        typedHighlights = infos[0] == null ? List.of() : Answers.highlights(infos[0]);
        PsiFile file = myFixture.getFile();
        List<String>[] refs = new List[2];
        rec.guard("final references bg", () -> refs[0] = background(() -> Answers.references(file, root)));
        rec.guard("final references edt", () -> refs[1] = Answers.references(file, root));
        typedRefs = refs[0];
        typedEdtRefs = refs[1];
        finalTexts.putAll(files);                         // the other files too (an .app file makes an include_lib resolve)
        for (String p : w.erlangPaths()) {
          PsiFile f = w.psi(p);
          if (f != null) finalTexts.put(p, f.getText());
        }
      }
      finally {
        w.dispose();
      }
      rec.setCaseFiles(finalTexts);
      // the same texts in a fresh project, twice
      List<String> freshRefs = null;
      List<String> freshHighlights = null;
      for (int run = 0; run < (smoke() ? 1 : 2); run++) {
        FuzzWorld fresh = world(finalTexts, false);
        try {
          PsiFile m = fresh.psi("m.erl");
          open(m.getVirtualFile());
          warm(m);
          List<HighlightInfo>[] infos = new List[1];
          rec.guard("fresh highlight", () -> infos[0] = myFixture.doHighlighting());
          List<String> h = infos[0] == null ? List.of() : Answers.highlights(infos[0]);
          PsiFile file = myFixture.getFile();
          String froot = fresh.root;
          List<String>[] refs = new List[1];
          rec.guard("fresh references bg", () -> refs[0] = background(() -> Answers.references(file, froot)));
          if (run == 0) {
            freshRefs = refs[0];
            freshHighlights = h;
          }
          else {
            compare("nondeterministic", "references", freshRefs, refs[0]);
            compare("nondeterministic", "highlights", freshHighlights, h);
          }
        }
        finally {
          fresh.dispose();
        }
      }
      compare("stale-answer", "references (background)", typedRefs, freshRefs);
      compare("stale-answer", "references (UI thread)", typedEdtRefs, freshRefs);
      compare("stale-answer", "highlights", typedHighlights, freshHighlights);
      for (List<String> l : Arrays.asList(typedRefs, typedEdtRefs, freshRefs)) {
        if (l != null && l.toString().contains("INVALID:")) rec.fail("wrong-answer", "invalid target in final answers", l.toString());
      }
  }

  private void compare(String kind, String what, List<String> a, List<String> b) {
    if (a == null || b == null || a.equals(b)) return;
    rec.fail(kind, what, Answers.diff(a, b));
  }

  /** One random edit; returns its name. */
  private String edit(FuzzWorld w, Random rnd, EditorActionProbe probe) {
    Document doc = myFixture.getEditor().getDocument();
    int len = doc.getTextLength();
    int offset = rnd.nextInt(len + 1);
    int kind = rnd.nextInt(10);
    String name;
    switch (kind) {
      case 0, 1, 2 -> {
        String s = SNIPPETS[rnd.nextInt(SNIPPETS.length)];
        name = "type " + s.replace("\n", "\\n") + " @" + offset;
        rec.guard(name, () -> {
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          myFixture.type(s);
        });
      }
      case 3 -> {
        int end = Math.min(len, offset + 1 + rnd.nextInt(12));
        name = "delete " + offset + ".." + end;
        rec.guard(name, () -> WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.deleteString(offset, end)));
      }
      case 4 -> {
        name = "backspace @" + offset;
        rec.guard(name, () -> {
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          myFixture.performEditorAction("EditorBackSpace");
        });
      }
      case 5 -> {
        name = "comment @" + offset;
        rec.guard(name, () -> {
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          myFixture.performEditorAction("CommentByLineComment");
        });
      }
      case 6 -> {
        name = "undo";
        probe.undo();
      }
      case 7 -> {
        name = "redo";
        probe.redo();
      }
      default -> {
        // an edit of another file: a header or b.erl (T1: answers in m.erl must follow)
        List<String> others = new ArrayList<>();
        for (String p : w.erlangPaths()) if (!p.equals("m.erl")) others.add(p);
        if (others.isEmpty()) return "none";
        String p = others.get(rnd.nextInt(others.size()));
        PsiFile f = w.psi(p);
        Document d = f == null ? null : PsiDocumentManager.getInstance(getProject()).getDocument(f);
        if (d == null) return "none";
        String text = d.getText();
        String changed = mutateOther(text, rnd);
        name = "edit-other " + p;
        rec.guard(name, () -> {
          WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(changed));
          if (rnd.nextBoolean()) PsiDocumentManager.getInstance(getProject()).commitDocument(d);
        });
      }
    }
    return name;
  }

  /** Deletes a line, renames a function, drops or adds a define. */
  private static String mutateOther(String text, Random rnd) {
    List<String> lines = new ArrayList<>(Arrays.asList(text.split("\n", -1)));
    switch (rnd.nextInt(4)) {
      case 0 -> {
        if (lines.size() > 1) lines.remove(rnd.nextInt(lines.size()));
      }
      case 1 -> {
        return text.replace("g()", "g_renamed()").replace("g/0", "g_renamed/0");
      }
      case 2 -> lines.add(Math.min(lines.size(), 1 + rnd.nextInt(Math.max(1, lines.size()))), "-define(C, 2).");
      default -> {
        return text.replaceAll("-record\\([a-z_0-9]+, \\{[^}]*\\}\\)\\.", "");
      }
    }
    return String.join("\n", lines);
  }

  /** A question at a random moment: at once, after the commit, during highlighting or after it; UI thread or background. */
  private void ask(FuzzWorld w, Random rnd, EditorActionProbe probe, String prefix) {
    PsiFile file = myFixture.getFile();
    int offset = rnd.nextInt(myFixture.getEditor().getDocument().getTextLength() + 1);
    switch (rnd.nextInt(5)) {
      case 0 -> rec.guard(prefix + "at-once edt", () -> refsAt(file, offset));
      case 1 -> {
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        rec.guard(prefix + "committed bg", () -> background(() -> { checkAll(file); return null; }));
      }
      case 2 -> {
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        // a background question while the UI thread highlights
        Future<?> f = ApplicationManager.getApplication().executeOnPooledThread(
          () -> ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
            if (file.isValid()) checkAll(file);
          }), new EmptyProgressIndicator()));
        rec.guard(prefix + "highlight-during-bg", () -> myFixture.doHighlighting());
        rec.guard(prefix + "bg-during-highlight", () -> PlatformTestUtil.waitForFuture(f, FuzzConfig.actionMillis()));
      }
      case 3 -> {
        rec.guard(prefix + "highlight", () -> myFixture.doHighlighting());
        rec.guard(prefix + "after-highlight edt", () -> checkAll(file));
      }
      default -> {
        myFixture.getEditor().getCaretModel().moveToOffset(Math.min(offset, myFixture.getEditor().getDocument().getTextLength()));
        probe.run(probe.random(rnd, true), rnd, prefix);
      }
    }
  }

  private void refsAt(PsiFile file, int offset) {
    PsiReference r = file.findReferenceAt(Math.min(offset, file.getTextLength()));
    if (r == null) return;
    var t = r.resolve();
    if (t != null && !t.isValid()) rec.fail("wrong-answer", "invalid resolve", r.getClass().getName() + " at " + offset);
  }

  private void checkAll(PsiFile file) {
    for (String s : Answers.references(file, "")) {
      if (s.contains("INVALID:")) rec.fail("wrong-answer", "invalid resolve " + s.substring(s.indexOf("INVALID:")), s);
    }
  }

  private <T> T background(java.util.concurrent.Callable<T> c) throws Exception {
    return new ElementProbe(rec, getProject(), null).background(c);
  }
}
