package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * Dumps what the plugin answers (references, highlighting, completion variants at sampled offsets) for generated projects
 * and the sample files into {@code MACRO_FUZZ_DUMP}/{@code <case>.txt}, so that two arms can be compared with a diff:
 * the branch with the layer off ({@code MACRO_FUZZ_LAYER=off}) against master, or the branch at two commits. Without
 * {@code MACRO_FUZZ_DUMP} it computes the answers of one case and writes nothing.
 */
public class MacroFuzzAnswersTest extends MacroFuzzTestBase {
  public void testDumpAnswers() throws Throwable {
    String dump = FuzzConfig.env("MACRO_FUZZ_DUMP");
    Map<String, String> samples = FuzzInputs.samples();
    List<String> sampleFiles = new ArrayList<>();
    for (String p : samples.keySet()) if (p.endsWith(".erl")) sampleFiles.add(p);
    EditorActionProbe.enableInspections(myFixture);
    int cases = dump == null ? 1 : FuzzConfig.isLongRun() ? Integer.MAX_VALUE : 40 + sampleFiles.size();
    runCases("answers", Math.min(cases, 100000), (index, rnd, caseId) -> {
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
      FuzzWorld w = world(files, false);
      if (sample) w.addIncludeRoot("include");
      StringBuilder out = new StringBuilder();
      try {
        PsiFile f = w.psi(main);
        open(f.getVirtualFile());
        warm(f);
        out.append("# ").append(caseId).append(' ').append(main).append(" arm=").append(LayerHooks.arm())
          .append(" layer=").append(LayerHooks.isEnabled()).append('\n');
        List<HighlightInfo>[] infos = new List[1];
        rec.guard("highlight", () -> infos[0] = myFixture.doHighlighting());
        out.append("## highlights\n");
        if (infos[0] != null) for (String s : Answers.highlights(infos[0])) out.append(s).append('\n');
        PsiFile file = myFixture.getFile();
        List<String>[] refs = new List[1];
        rec.guard("references", () -> refs[0] = new ElementProbe(rec, getProject(), null).background(() -> Answers.references(file, w.root)));
        out.append("## references\n");
        if (refs[0] != null) for (String s : refs[0]) out.append(s).append('\n');
        out.append("## completion\n");
        List<Integer> offsets = TargetProbe.offsets(file, rnd, 4);
        Collections.shuffle(offsets, rnd);
        String text = myFixture.getEditor().getDocument().getText();
        for (int offset : offsets.subList(0, Math.min(6, offsets.size()))) {
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          List<String>[] items = new List[1];
          rec.guard("complete @" + offset, () -> {
            myFixture.completeBasic();
            List<String> strings = myFixture.getLookupElementStrings();
            items[0] = strings == null ? List.of("<single or none>") : new ArrayList<>(new TreeSet<>(strings));
            LookupManager.hideActiveLookup(getProject());
          });
          out.append(offset).append(' ').append(items[0]).append('\n');
          if (!myFixture.getEditor().getDocument().getText().equals(text)) {
            com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> myFixture.getEditor().getDocument().setText(text));
            PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
          }
        }
      }
      finally {
        w.dispose();
      }
      if (dump != null) {
        Path p = Paths.get(dump);
        Files.createDirectories(p);
        Files.writeString(p.resolve(caseId.replace(':', '_') + ".txt"), out.toString(), StandardCharsets.UTF_8);
      }
    });
  }
}
