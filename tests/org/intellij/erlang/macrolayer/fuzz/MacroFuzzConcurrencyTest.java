package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.DumbModeTestUtils;
import com.intellij.testFramework.PlatformTestUtil;

import java.util.*;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Concurrency and state: questions without read access, in dumb mode, and on background threads while the UI thread
 * types (with indicators cancelled at random). Afterwards the answers must equal those of a fresh project with the same
 * texts (no kept empty or dumb answer).
 */
public class MacroFuzzConcurrencyTest extends MacroFuzzTestBase {
  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  /** The answers of a fresh project with these texts (references in the background, after warming and highlighting). */
  private List<String> freshAnswers(Map<String, String> texts) {
    FuzzWorld fresh = world(texts, false);
    try {
      PsiFile m = fresh.psi("m.erl");
      open(m.getVirtualFile());
      warm(m);
      rec.guard("fresh highlight", () -> myFixture.doHighlighting());
      PsiFile file = myFixture.getFile();
      List<String>[] r = new List[1];
      rec.guard("fresh references", () -> r[0] = background(() -> Answers.references(file, fresh.root)));
      return r[0];
    }
    finally {
      fresh.dispose();
    }
  }

  private <T> T background(java.util.concurrent.Callable<T> c) throws Exception {
    return new ElementProbe(rec, getProject(), null).background(c);
  }

  private void compare(String what, List<String> a, List<String> b) {
    if (a != null && b != null && !a.equals(b)) rec.fail("stale-answer", what, Answers.diff(a, b));
  }

  public void testNoReadAccessThenSameAnswers() throws Throwable {
    runCases("noread", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new MacroCaseGenerator(rnd).project();
      FuzzWorld w = world(files, false);
      List<String> after;
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        // no warming: the first questions come without read access, before any copy exists
        List<PsiReference> refs = new ArrayList<>();
        for (PsiElement e : PsiTreeUtil.collectElements(m, x -> true)) refs.addAll(Arrays.asList(e.getReferences()));
        Collections.shuffle(refs, rnd);
        List<PsiReference> some = refs.subList(0, Math.min(refs.size(), smoke() ? 15 : 60));
        // the platform asserts read access (a logged error) in master's code too; only what the layer throws counts here
        rec.guardSuppressing("Read access is allowed", "no-read-access resolve", () -> {
          List<Throwable> thrown = Collections.synchronizedList(new ArrayList<>());
          Future<?> f = ApplicationManager.getApplication().executeOnPooledThread(() -> {
            for (PsiReference r : some) {
              try {
                r.resolve();
                if (r instanceof PsiPolyVariantReference pv) pv.multiResolve(false);
              }
              catch (Throwable t) {
                // the platform may refuse a read without read access; the layer itself must not throw
                if (String.valueOf(FuzzRecorder.pluginFrames(t)).contains("MacroLayer")) thrown.add(t);
              }
            }
          });
          PlatformTestUtil.waitForFuture(f, FuzzConfig.actionMillis());
          if (!thrown.isEmpty()) throw thrown.get(0);
        });
        warm(m);
        rec.guard("highlight", () -> myFixture.doHighlighting());
        List<String>[] r = new List[1];
        rec.guard("references after", () -> r[0] = background(() -> Answers.references(m, w.root)));
        after = r[0];
      }
      finally {
        w.dispose();
      }
      compare("answers after questions without read access", after, freshAnswers(files));
    });
  }

  public void testDumbModeThenSameAnswers() throws Throwable {
    EditorActionProbe.enableInspections(myFixture);
    runCases("dumb", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new MacroCaseGenerator(rnd).project();
      FuzzWorld w = world(files, false);
      List<String> after;
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
        TargetProbe targets = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
        targets.searchedTargets = 0;   // a search waits for smart mode: never inside dumb mode
        List<Integer> offsets = TargetProbe.offsets(myFixture.getFile(), rnd, 5);
        Collections.shuffle(offsets, rnd);
        List<Integer> some = offsets.subList(0, Math.min(offsets.size(), smoke() ? 4 : 20));
        DumbModeTestUtils.runInDumbModeSynchronously(getProject(), () -> {
          rec.guard("dumb warm", () -> background(() -> LayerHooks.copyFile(m)));
          for (int offset : some) {
            myFixture.getEditor().getCaretModel().moveToOffset(offset);
            probe.run(probe.random(rnd, true, EditorActionProbe.NOT_IN_DUMB_MODE), rnd, "dumb @" + offset + " ");
            targets.probeOffset(offset, rnd.nextBoolean());
          }
        });
        warm(m);
        rec.guard("highlight", () -> myFixture.doHighlighting());
        List<String>[] r = new List[1];
        rec.guard("references after", () -> r[0] = background(() -> Answers.references(m, w.root)));
        after = r[0];
      }
      finally {
        w.dispose();
      }
      compare("answers after dumb mode", after, freshAnswers(files));
    });
  }

  public void testBackgroundQueriesWhileTyping() throws Throwable {
    runCases("interleave", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new LinkedHashMap<>(new MacroCaseGenerator(rnd).project());
      FuzzWorld w = world(files, false);
      Map<String, String> finalTexts = new LinkedHashMap<>();
      List<String> after;
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        AtomicBoolean stop = new AtomicBoolean();
        List<ProgressIndicator> indicators = Collections.synchronizedList(new ArrayList<>());
        List<Throwable> bgErrors = Collections.synchronizedList(new ArrayList<>());
        Random bgRnd = new Random(rnd.nextLong());
        Future<?> bg = ApplicationManager.getApplication().executeOnPooledThread(() -> {
          while (!stop.get()) {
            EmptyProgressIndicator ind = new EmptyProgressIndicator();
            indicators.add(ind);
            try {
              ProgressManager.getInstance().runProcess(() -> ReadAction.run(() -> {
                PsiFile f = PsiDocumentManager.getInstance(getProject()).getPsiFile(myFixture.getEditor().getDocument());
                if (f == null || !f.isValid()) return;
                int kind = bgRnd.nextInt(3);
                if (kind == 0) LayerHooks.copyFile(f);
                for (String s : Answers.references(f, "")) {
                  if (s.contains("INVALID:")) bgErrors.add(new AssertionError("invalid answer while typing: " + s));
                }
              }), ind);
            }
            catch (Throwable t) {
              if (!FuzzRecorder.isCancellation(t)) bgErrors.add(t);
            }
          }
        });
        try {
          String[] snippets = {"?", "X", "(", ")", ",", " ", "\n", "?C", "foo", ".", "-define(Q, 1).\n", "?Q"};
          int steps = smoke() ? 25 : 200;
          for (int i = 0; i < steps; i++) {
            int len = myFixture.getEditor().getDocument().getTextLength();
            int offset = rnd.nextInt(len + 1);
            String s = snippets[rnd.nextInt(snippets.length)];
            rec.guard("type " + s.replace("\n", "\\n") + " @" + offset, () -> {
              myFixture.getEditor().getCaretModel().moveToOffset(offset);
              myFixture.type(s);
            });
            if (rnd.nextInt(3) == 0) PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
            if (rnd.nextInt(4) == 0) {
              synchronized (indicators) {
                if (!indicators.isEmpty()) indicators.get(indicators.size() - 1).cancel();
              }
            }
            if (rnd.nextInt(10) == 0) rec.guard("highlight while bg", () -> myFixture.doHighlighting());
          }
        }
        finally {
          stop.set(true);
          rec.guard("background loop ends", () -> PlatformTestUtil.waitForFuture(bg, FuzzConfig.actionMillis()));
        }
        for (Throwable t : bgErrors) {
          rec.guard("background question", () -> {
            throw t;
          });
        }
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
        warm(m);
        rec.guard("highlight", () -> myFixture.doHighlighting());
        PsiFile file = myFixture.getFile();
        List<String>[] r = new List[1];
        rec.guard("references after", () -> r[0] = background(() -> Answers.references(file, w.root)));
        after = r[0];
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
      compare("answers after typing with background questions", after, freshAnswers(finalTexts));
    });
  }
}
