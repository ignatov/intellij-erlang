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
    // MACRO_FUZZ_DUMP_CASES: the number of cases of a dump (default: every sample file and 40 generated projects)
    String n = FuzzConfig.env("MACRO_FUZZ_DUMP_CASES");
    int cases = dump == null ? 1 : n != null ? Integer.parseInt(n) : FuzzConfig.isLongRun() ? Integer.MAX_VALUE : 40 + sampleFiles.size();
    runCases("answers", Math.min(cases, 100000), (index, rnd, caseId) -> {
      String out = AnswerDump.compute(this, index, rnd, caseId, samples, sampleFiles);
      if (dump != null) {
        Path p = Paths.get(dump);
        Files.createDirectories(p);
        Files.writeString(p.resolve(caseId.replace(':', '_') + ".txt"), out, StandardCharsets.UTF_8);
      }
    });
  }

  /** The answers of case {@code index}, as {@link #testDumpAnswers} writes them. */
  protected String answersOf(int index, Random rnd, String caseId, Map<String, String> samples, List<String> sampleFiles) {
    return AnswerDump.compute(this, index, rnd, caseId, samples, sampleFiles);
  }
}
