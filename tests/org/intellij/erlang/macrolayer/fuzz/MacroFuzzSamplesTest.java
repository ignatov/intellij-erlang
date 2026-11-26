package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.openapi.actionSystem.ActionPlaces;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;

import java.util.*;

/**
 * The sample project the owner clicks around in (a snapshot in {@code testData/macroFuzz/erl-test}, or the live folder
 * with {@code MACRO_FUZZ_SAMPLES}): every editor action at the offsets of every file, and the Project view popup on every
 * node of its tree. A case is one file (the case index modulo the number of files); the smoke run takes three fixed
 * files and a sample of offsets, a long run every offset.
 */
public class MacroFuzzSamplesTest extends MacroFuzzTestBase {
  private static final List<String> SMOKE_FILES = List.of("src/one_file/m_fragments.erl", "src/one_file/m_made_by_macro.erl",
                                                          "include/chain_mid.hrl");

  private FuzzWorld sampleWorld(Map<String, String> files) {
    FuzzWorld w = world(files, false);
    w.addIncludeRoot("include");
    return w;
  }

  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  public void testEditorActionsAtOffsets() throws Throwable {
    Map<String, String> files = FuzzInputs.samples();
    List<String> sources = new ArrayList<>();
    for (String p : files.keySet()) if (p.endsWith(".erl") || p.endsWith(".hrl")) sources.add(p);
    if (sources.isEmpty()) return;
    EditorActionProbe.enableInspections(myFixture);
    runCases("samples-editor", SMOKE_FILES.size(), (index, rnd, caseId) -> {
      String path = smoke() && SMOKE_FILES.size() > index && files.containsKey(SMOKE_FILES.get(index))
                    ? SMOKE_FILES.get(index) : sources.get(index % sources.size());
      FuzzWorld w = sampleWorld(files);
      try {
        rec.guard("file " + path, () -> {});
        editorActionsOnFile(w, path, rnd, smoke() ? 12 : Integer.MAX_VALUE);
      }
      finally {
        w.dispose();
      }
    });
  }

  /** Every read-only action at each offset, editing actions at some, the per-file actions once. */
  void editorActionsOnFile(FuzzWorld w, String path, Random rnd, int maxOffsets) {
    PsiFile f = w.psi(path);
    if (f == null) return;
    open(f.getVirtualFile());
    warm(myFixture.getFile());
    EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
    Map<String, EditorActionProbe.Act> byName = new LinkedHashMap<>();
    for (EditorActionProbe.Act a : probe.acts()) byName.put(a.name(), a);
    for (String once : List.of("highlight", "structure-view", "folding", "goto-symbol")) probe.run(byName.get(once), rnd, "");
    Document doc = myFixture.getEditor().getDocument();
    String original = doc.getText();
    List<Integer> offsets = TargetProbe.offsets(myFixture.getFile(), rnd, maxOffsets == Integer.MAX_VALUE ? 100000 : maxOffsets / 2);
    if (offsets.size() > maxOffsets) {
      Collections.shuffle(offsets, rnd);
      offsets = new ArrayList<>(offsets.subList(0, maxOffsets));
      Collections.sort(offsets);
    }
    TargetProbe targets = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
    targets.searchedTargets = smoke() ? 2 : 6;
    for (int offset : offsets) {
      String p = "@" + offset + " ";
      myFixture.getEditor().getCaretModel().moveToOffset(offset);
      myFixture.getEditor().getSelectionModel().removeSelection();
      for (EditorActionProbe.Act a : probe.acts()) {
        if (!a.readOnly() || List.of("highlight", "structure-view", "folding", "goto-symbol").contains(a.name())) continue;
        if (smoke() && rnd.nextInt(3) != 0) continue;
        myFixture.getEditor().getCaretModel().moveToOffset(offset);
        probe.run(a, rnd, p);
      }
      targets.probeOffset(offset, false);
      targets.probeOffset(offset, true);
      // one editing action at this offset, then the original text back
      EditorActionProbe.Act edit = probe.random(rnd, false);
      if (!edit.readOnly()) {
        myFixture.getEditor().getCaretModel().moveToOffset(Math.min(offset, doc.getTextLength()));
        probe.run(edit, rnd, p);
        probe.checkConsistency(p + edit.name() + " ");
        if (!doc.getText().equals(original)) {
          WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.setText(original));
          PsiDocumentManager.getInstance(getProject()).commitDocument(doc);
        }
      }
    }
    probe.run(byName.get("highlight"), rnd, "end ");
  }

  public void testProjectViewPopupOnEveryNode() throws Throwable {
    Map<String, String> files = FuzzInputs.samples();
    if (files.isEmpty()) return;
    runCases("samples-popup", 1, (index, rnd, caseId) -> {
      FuzzWorld w = sampleWorld(files);
      try {
        ActionUpdateProbe probe = new ActionUpdateProbe(rec, getProject(), getModule());
        List<VirtualFile> nodes = new ArrayList<>();
        VfsUtilCore.iterateChildrenRecursively(w.rootDir, null, vf -> {
          nodes.add(vf);
          return true;
        });
        PsiManager pm = PsiManager.getInstance(getProject());
        List<PsiElement> all = new ArrayList<>();
        for (VirtualFile vf : nodes) {
          PsiElement e = vf.isDirectory() ? pm.findDirectory(vf) : pm.findFile(vf);
          if (e != null) all.add(e);
        }
        all.add(w.sourceRoot());
        for (PsiElement e : all) {
          String name = e instanceof PsiFile pf ? pf.getVirtualFile().getPath() : ((com.intellij.psi.PsiDirectory)e).getVirtualFile().getPath();
          name = name.substring(name.indexOf(w.root) < 0 ? 0 : name.indexOf(w.root));
          DataContext ctx = probe.selection(List.of(e), null);
          // the popup itself, as a right click opens it; the main menu and the other groups in long runs
          if (smoke()) {
            expandOne(probe, "node[" + name + "]", ctx);
          }
          else {
            probe.expandGroups("node[" + name + "]", ctx, ActionPlaces.PROJECT_VIEW_POPUP);
            probe.updateAll("node[" + name + "]", ctx, ActionPlaces.PROJECT_VIEW_POPUP, rnd, 0);
          }
        }
        // several nodes at once
        for (int i = 0; i < (smoke() ? 2 : 20); i++) {
          List<PsiElement> sel = new ArrayList<>();
          int n = 2 + rnd.nextInt(4);
          for (int j = 0; j < n; j++) sel.add(all.get(rnd.nextInt(all.size())));
          expandOne(probe, "nodes[" + n + "]#" + i, probe.selection(sel, null));
        }
      }
      finally {
        w.dispose();
      }
    });
  }

  private void expandOne(ActionUpdateProbe probe, String label, DataContext ctx) {
    probe.expandGroups(label, ctx, ActionPlaces.PROJECT_VIEW_POPUP, "ProjectViewPopupMenu");
  }
}
