package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;

import java.util.*;

/** The answers of one case of {@link MacroFuzzAnswersTest}, as text: highlights, references, completion, the inputs. */
final class AnswerDump {
  private AnswerDump() {
  }

  static String compute(MacroFuzzTestBase t, int index, Random rnd, String caseId, Map<String, String> samples, List<String> sampleFiles) {
      Map<String, String> files;
      String main;
      boolean sample = index % 2 == 1 && index / 2 < sampleFiles.size();
      if (sample) {
        files = samples;
        main = sampleFiles.get(index / 2);
      }
      else {
        files = new MacroCaseGenerator(rnd).project();
        main = "m.erl";
      }
      FuzzWorld w = t.world(files, false);
      if (sample) w.addIncludeRoot("include");
      StringBuilder out = new StringBuilder();
      try {
        PsiFile f = w.psi(main);
        t.open(f.getVirtualFile());
        t.warm(f);
        out.append("# ").append(caseId).append(' ').append(main).append(" arm=").append(LayerHooks.arm())
          .append(" layer=").append(LayerHooks.isEnabled()).append('\n');
        List<HighlightInfo>[] infos = new List[1];
        t.rec.guard("highlight", () -> infos[0] = t.fixture().doHighlighting());
        out.append("## highlights\n");
        if (infos[0] != null) for (String s : Answers.highlights(infos[0])) out.append(s).append('\n');
        PsiFile file = t.fixture().getFile();
        List<String>[] refs = new List[1];
        t.rec.guard("references", () -> refs[0] = new ElementProbe(t.rec, t.fixture().getProject(), null).background(() -> Answers.references(file, w.root)));
        out.append("## references\n");
        if (refs[0] != null) for (String s : refs[0]) out.append(s).append('\n');
        out.append("## completion\n");
        List<Integer> offsets = TargetProbe.offsets(file, rnd, 4);
        Collections.shuffle(offsets, rnd);
        String text = t.fixture().getEditor().getDocument().getText();
        for (int offset : offsets.subList(0, Math.min(6, offsets.size()))) {
          t.fixture().getEditor().getCaretModel().moveToOffset(offset);
          List<String>[] items = new List[1];
          t.rec.guard("complete @" + offset, () -> {
            t.fixture().completeBasic();
            List<String> strings = t.fixture().getLookupElementStrings();
            items[0] = strings == null ? List.of("<single or none>") : new ArrayList<>(new TreeSet<>(strings));
            LookupManager.hideActiveLookup(t.fixture().getProject());
          });
          out.append(offset).append(' ').append(items[0]).append('\n');
          if (!t.fixture().getEditor().getDocument().getText().equals(text)) {
            com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(t.fixture().getProject(), () -> t.fixture().getEditor().getDocument().setText(text));
            PsiDocumentManager.getInstance(t.fixture().getProject()).commitAllDocuments();
          }
        }
      }
      finally {
        w.dispose();
      }
      if (!sample) {
        // the inputs, so that a difference can be checked against the real preprocessor
        for (Map.Entry<String, String> e : files.entrySet()) {
          out.append("## file ").append(e.getKey()).append('\n').append(e.getValue());
          if (!e.getValue().endsWith("\n")) out.append('\n');
        }
      }
      return out.toString();
  }
}
