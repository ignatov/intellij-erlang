package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;

import java.util.*;

/**
 * The layer's switch flipped in the middle of a session (the Registry key {@code erlang.macro.layer}): after each flip,
 * with editor actions and edits (of the open file and of another file) in between, the answers (references in the
 * background and on the UI thread, highlighting) must equal those of a fresh project with the same texts in that mode;
 * no answer may be kept from the other mode.
 */
public class MacroFuzzSwitchTest extends MacroFuzzTestBase {
  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  private record State(List<String> bg, List<String> edt, List<String> highlights) {
  }

  private State answers(FuzzWorld w, String main) {
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    List<HighlightInfo>[] infos = new List[1];
    rec.guard("highlight", () -> infos[0] = myFixture.doHighlighting());
    PsiFile file = myFixture.getFile();
    List<String>[] r = new List[2];
    rec.guard("references bg", () -> r[0] = new ElementProbe(rec, getProject(), null).background(() -> Answers.references(file, w.root)));
    rec.guard("references edt", () -> r[1] = Answers.references(file, w.root));
    return new State(r[0], r[1], infos[0] == null ? List.of() : Answers.highlights(infos[0]));
  }

  private State fresh(Map<String, String> texts, boolean on) {
    Disposable d = Disposer.newDisposable();
    LayerHooks.setEnabled(on, d);
    FuzzWorld w = world(texts, false);
    try {
      PsiFile m = w.psi("m.erl");
      open(m.getVirtualFile());
      warm(m);
      return answers(w, "m.erl");
    }
    finally {
      w.dispose();
      Disposer.dispose(d);
    }
  }

  private void compare(String what, State typed, State fresh) {
    for (int i = 0; i < 3; i++) {
      List<String> a = i == 0 ? typed.bg : i == 1 ? typed.edt : typed.highlights;
      List<String> b = i == 0 ? fresh.bg : i == 1 ? fresh.bg : fresh.highlights;
      String part = i == 0 ? "references (background)" : i == 1 ? "references (UI thread)" : "highlights";
      if (a != null && b != null && !a.equals(b)) rec.fail("stale-answer", what + ": " + part, Answers.diff(a, b));
    }
  }

  public void testFlipsAgreeWithFreshRunInEachMode() throws Throwable {
    if (!LayerHooks.present()) return;
    EditorActionProbe.enableInspections(myFixture);
    runCases("switch", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new LinkedHashMap<>(new MacroCaseGenerator(rnd).project());
      if (!files.containsKey("b.erl")) files.put("b.erl", "-module(b).\n-export([g/0]).\ng() -> ok.\n");
      List<Boolean> modes = new ArrayList<>();
      List<State> typed = new ArrayList<>();
      List<Map<String, String>> texts = new ArrayList<>();
      Disposable session = Disposer.newDisposable();
      FuzzWorld w = world(files, false);
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        warm(m);
        EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
        boolean on = true;
        Disposable phase = Disposer.newDisposable();
        int flips = smoke() ? 2 : 3 + rnd.nextInt(5);
        for (int flip = 0; flip < flips; flip++) {
          // some actions and edits in the current mode
          for (int k = 0; k < (smoke() ? 2 : 5); k++) {
            int len = myFixture.getEditor().getDocument().getTextLength();
            myFixture.getEditor().getCaretModel().moveToOffset(rnd.nextInt(len + 1));
            probe.run(probe.random(rnd, rnd.nextInt(3) != 0), rnd, "mode " + on + " ");
            if (rnd.nextInt(3) == 0) editOther(w, rnd);
          }
          // flip
          Disposer.dispose(phase);
          phase = Disposer.newDisposable();
          on = !on;
          LayerHooks.setEnabled(on, phase);
          rec.guard("flip to " + (on ? "on" : "off"), () -> {});
          State s = answers(w, "m.erl");
          Map<String, String> t = new LinkedHashMap<>(files);
          for (String p : w.erlangPaths()) {
            PsiFile f = w.psi(p);
            if (f != null) t.put(p, f.getText());
          }
          modes.add(on);
          typed.add(s);
          texts.add(t);
        }
        Disposer.dispose(phase);
      }
      finally {
        w.dispose();
        Disposer.dispose(session);
      }
      for (int i = 0; i < typed.size(); i++) {
        rec.setCaseFiles(texts.get(i));
        compare("after flip " + (i + 1) + " to " + (modes.get(i) ? "on" : "off"), typed.get(i), fresh(texts.get(i), modes.get(i)));
      }
    });
  }

  private void editOther(FuzzWorld w, Random rnd) {
    String p = rnd.nextBoolean() ? "b.erl" : "h1.hrl";
    PsiFile f = w.psi(p);
    if (f == null) return;
    var d = PsiDocumentManager.getInstance(getProject()).getDocument(f);
    if (d == null) return;
    String text = d.getText();
    String changed = switch (rnd.nextInt(3)) {
      case 0 -> text.replace("g()", "g2()").replace("g/0", "g2/0");
      case 1 -> text + "\n-define(ADDED" + rnd.nextInt(9) + ", 1).\n";
      default -> text.contains("\n") ? text.substring(0, text.lastIndexOf('\n', Math.max(0, text.length() - 2)) + 1) : text;
    };
    rec.guard("edit-other " + p, () -> com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(changed)));
  }
}
