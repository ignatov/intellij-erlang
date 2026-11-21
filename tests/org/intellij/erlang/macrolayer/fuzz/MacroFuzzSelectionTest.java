package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.actionSystem.ActionPlaces;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

import java.util.*;

/**
 * Families 2 and 3: actions updated with the selections a user can make (directories, files of every kind, several
 * nodes, editor contexts at random offsets), and elements of every kind pushed through the platform APIs that reach
 * plugin code. See {@link FuzzConfig} for the knobs.
 */
public class MacroFuzzSelectionTest extends MacroFuzzTestBase {
  public void testActionUpdateOnSelections() throws Throwable {
    runCases("selection", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new MacroCaseGenerator(rnd).project();
      FuzzWorld w = world(files, true);
      try {
        ActionUpdateProbe probe = new ActionUpdateProbe(rec, getProject(), getModule());
        List<Pair<String, List<PsiElement>>> selections = new ArrayList<>();
        for (Pair<String, PsiElement> e : ElementZoo.nonErlang(w)) {
          if (e.first.startsWith("nonphysical")) continue;
          selections.add(Pair.create(e.first, List.of(e.second)));
        }
        PsiFile m = w.psi("m.erl");
        PsiFile h = w.psi("h1.hrl");
        selections.add(Pair.create("file m.erl", List.of(m)));
        selections.add(Pair.create("file h1.hrl", List.of(h)));
        selections.add(Pair.create("several", List.of(m, w.dir("empty/pkg"), w.psi("notes.txt"), w.dir(""))));
        selections.add(Pair.create("two-dirs", List.of(w.dir("zz_empty"), w.dir("empty"))));
        boolean smoke = !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
        if (smoke) {
          // keep the smoke run short: the directories and two files, the full set in long runs
          selections.removeIf(s -> !(s.first.startsWith("dir") || s.first.contains("m.erl") || s.first.contains("beam") || s.first.equals("several")));
        }
        int max = smoke ? 120 : 0;
        for (Pair<String, List<PsiElement>> s : selections) {
          DataContext ctx = probe.selection(s.second, null);
          probe.expandGroups("sel[" + s.first + "]", ctx, ActionPlaces.PROJECT_VIEW_POPUP);
          probe.updateAll("sel[" + s.first + "]", ctx, ActionPlaces.PROJECT_VIEW_POPUP, rnd, max);
        }
        // editor contexts at random offsets of the main module
        open(m.getVirtualFile());
        warm(m);
        int len = myFixture.getEditor().getDocument().getTextLength();
        for (int i = 0; i < (smoke ? 1 : 4); i++) {
          int offset = rnd.nextInt(len + 1);
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          PsiElement at = myFixture.getFile().findElementAt(Math.max(0, Math.min(offset, len - 1)));
          DataContext ctx = probe.selection(at == null ? List.of() : List.of(at), myFixture.getEditor());
          probe.expandGroups("editor@" + offset, ctx, ActionPlaces.EDITOR_POPUP);
          probe.updateAll("editor@" + offset, ctx, ActionPlaces.EDITOR_POPUP, rnd, max);
        }
      }
      finally {
        w.dispose();
      }
    });
  }

  public void testEntryPointsWithArbitraryElements() throws Throwable {
    runCases("entry", 2, (index, rnd, caseId) -> {
      Map<String, String> files = new LinkedHashMap<>(new MacroCaseGenerator(rnd).project());
      files.put("inval.erl", "-module(inval).\n-define(I(X), X + 1).\nf(Y) -> ?I(Y).\n");
      files.put("lib.erl", "-module(lib).\n-export([g/0]).\n-define(L, 1).\n-record(lr, {a = ?L}).\ng() -> ?L.\n");
      FuzzWorld w = world(files, true);
      try {
        PsiFile m = w.psi("m.erl");
        open(m.getVirtualFile());
        ElementProbe probe = new ElementProbe(rec, getProject(), myFixture.getEditor());
        boolean smoke = !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
        List<Pair<String, PsiElement>> zoo = new ArrayList<>(ElementZoo.stubbed(w.psi("lib.erl")));
        zoo.addAll(ElementZoo.nonErlang(w));
        zoo.addAll(ElementZoo.erlang("m", m, rnd, smoke ? 10 : 40));
        zoo.addAll(ElementZoo.erlang("h1", w.psi("h1.hrl"), rnd, 5));
        PsiFile np = ElementZoo.nonPhysicalErlang(getProject(), "np.erl", files.get("m.erl"));
        zoo.addAll(ElementZoo.erlang("nonphysical", np, rnd, smoke ? 5 : 20));
        zoo.addAll(ElementZoo.erlang("copy", (PsiFile)m.copy(), rnd, smoke ? 5 : 20));
        warm(m);
        PsiFile layerCopy = LayerHooks.copyFile(m);
        if (layerCopy != null) zoo.addAll(ElementZoo.erlang("layer-copy", layerCopy, rnd, smoke ? 5 : 20));
        zoo.addAll(ElementZoo.invalidated(w.psi("inval.erl"), rnd));
        for (Pair<String, PsiElement> e : zoo) {
          probe.searches = !smoke || rnd.nextInt(4) == 0;
          // elements of other kinds get every entry point; Erlang elements (not targets) and invalid ones the general ones
          boolean full = ElementZoo.isOtherKind(e.first);
          probe.probe(e.first, e.second, full);
        }
      }
      finally {
        w.dispose();
      }
    });
  }
}
