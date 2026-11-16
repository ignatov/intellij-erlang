/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.elp.lsp;

import com.intellij.codeInsight.daemon.impl.DaemonProgressIndicator;
import com.intellij.codeInspection.InspectionEngine;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemDescriptorBase;
import com.intellij.codeInspection.ex.LocalInspectionToolWrapper;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiFile;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.Range;
import org.intellij.erlang.elp.ElpDiagnosticCounterparts;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.inspection.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Proves each pair of {@link ElpDiagnosticCounterparts} with the diagnostics the real ELP published for the planted defects of the
 * fixture project: ELP reports the problem with a mapped code, the paired plugin inspection reports it at the same place in the
 * {@code Plugin} position, stays silent on the fly while ELP serves the file, and still reports it in batch mode.
 */
public class ElpRecordedDiagnosticCounterpartsTest extends ElpRecordedProjectTestBase {
  /** The defect behind each pair: a file and a needle with {@code |} inside the code that both report. */
  private static final Map<Class<? extends ErlangInspectionBase>, List<String>> DEFECTS = new LinkedHashMap<>();

  private static void defect(@NotNull Class<? extends ErlangInspectionBase> inspection, String @NotNull ... needles) {
    DEFECTS.put(inspection, List.of(needles));
  }

  static {
    String defects = "src/shop_defects.erl:";
    defect(ErlangUnboundVariableInspection.class, defects + "unbound() -> Unb|ound.");
    defect(ErlangUnresolvedFunctionInspection.class, defects + "calls_missing() -> missing_lo|cal(1).",
           defects + "shop_memory:no_such_func|tion(1)");
    defect(ErlangUnresolvedExportFunctionInspection.class, defects + "undefined_exp|ort/0");
    defect(ErlangUnusedFunctionInspection.class, defects + "\nunused_func|tion() -> ok.");
    defect(ErlangUnusedVariableInspection.class, defects + "unused_variable(Unu|sed)");
    defect(ErlangUnusedTypeInspection.class, defects + "-type unused_ty|pe()");
    defect(ErlangUnresolvedRecordInspection.class, defects + "#no_such_rec|ord{}");
    defect(ErlangUnresolvedRecordFieldInspection.class, defects + "#item{no_such_fi|eld = 1}");
    defect(ErlangUnresolvedMacrosInspection.class, defects + "?NO_SUCH_MA|CRO.", defects + "?NO_SUCH_MACRO_CA|LL(1)");
    defect(ErlangUnresolvedIncludeInspection.class, defects + "-include(\"no_such_hea|der.hrl\").");
    defect(ErlangUnresolvedIncludeLibInspection.class, defects + "-include_lib(\"no_such_app/include/no_such_hea|der.hrl\").");
    defect(ErlangSpecificationForUndefinedFunctionInspection.class, defects + "-spec no_such_func|tion() -> ok.");
    defect(ErlangHeadMismatchInspection.class, defects + "\nmismatch|ed(2) -> two.");
    defect(ErlangDuplicateFunctionInspection.class, defects + "duplicate() -> 1.\ndupli|cate() -> 2.");
    defect(ErlangDefiningImportedFunctionInspection.class, defects + "\nrever|se(List) -> List.");
    defect(ErlangAmbiguousCallOfAutoImportedFunctionInspection.class, defects + "ambiguous(Term) -> si|ze(Term).");
    defect(ErlangImportDirectiveOverridesAutoImportedBifInspection.class, defects + "-import(shop_memory, [len|gth/1]).");
    defect(ErlangFunctionAlreadyImportedInspection.class, defects + "-import(shop_backend, [ma|p/2]).");
    defect(ErlangDuplicateFunctionExportInspection.class, defects + "duplicate/0, dupli|cate/0])");
    defect(ErlangIllegalPatternInspection.class, defects + "illegal_pattern(X |+ 1)");
    defect(ErlangIllegalGuardInspection.class, defects + "when lists:mem|ber(X, [1, 2])");
    defect(ErlangIoFormatInspection.class, defects + "io:format(\"~p ~p~n\", [o|ne])");
    defect(ErlangIncorrectModuleNameInspection.class, "src/shop_misnamed.erl:-module(shop_misnamed_mo|dule).");
  }

  public void testEveryPairHasAPlantedDefect() {
    assertEquals(ElpDiagnosticCounterparts.TABLE.keySet(), DEFECTS.keySet());
  }

  public void testElpReportsEveryPlantedDefectWithAMappedCode() {
    Map<String, List<Diagnostic>> diagnostics = myAnswers.diagnostics();
    for (var entry : DEFECTS.entrySet()) {
      List<String> codes = ElpDiagnosticCounterparts.TABLE.get(entry.getKey()).elpCodes();
      for (String defect : entry.getValue()) {
        assertFalse(entry.getKey().getSimpleName() + " " + defect, elpDiagnostics(diagnostics, defect, codes).isEmpty());
      }
    }
  }

  /**
   * The plugin reports the same defect at the same place: its problem holds the defect and overlaps ELP's diagnostic or starts on
   * its line. The plugin marks a wrong format string where ELP marks the argument list after it.
   */
  public void testThePluginReportsTheSameProblemInThePluginPosition() throws Exception {
    position(ElpMode.PLUGIN);
    Map<String, List<Diagnostic>> diagnostics = myAnswers.diagnostics();
    List<String> mismatches = new ArrayList<>();
    for (var entry : DEFECTS.entrySet()) {
      List<String> codes = ElpDiagnosticCounterparts.TABLE.get(entry.getKey()).elpCodes();
      for (String defect : entry.getValue()) {
        List<TextRange> plugin = problems(entry.getKey(), fileOf(defect), true);
        List<TextRange> elp = new ArrayList<>();
        for (Diagnostic diagnostic : elpDiagnostics(diagnostics, defect, codes)) elp.add(range(fileOf(defect), diagnostic.getRange()));
        boolean same = false;
        for (TextRange pluginRange : plugin) {
          for (TextRange elpRange : elp) {
            same |= pluginRange.intersects(elpRange) && pluginRange.containsOffset(offsetOf(defect)) ||
                    line(fileOf(defect), pluginRange.getStartOffset()) == line(fileOf(defect), elpRange.getStartOffset());
          }
        }
        if (!same) mismatches.add(entry.getKey().getSimpleName() + " " + defect + ": plugin " + plugin + ", ELP " + elp);
      }
    }
    assertEmpty(mismatches);
  }

  /** While ELP serves the file, the inspections of the pairs are silent on the fly, and batch inspection still runs them. */
  /** Under a served root ELP's diagnostics replace the plugin's inspections on the fly and in batch. */
  public void testSilentOnTheFlyAndInBatchUnderAServedRoot() throws Exception {
    for (var entry : DEFECTS.entrySet()) {
      String file = fileOf(entry.getValue().getFirst());
      position(ElpMode.PLUGIN);
      List<TextRange> pluginOnTheFly = problems(entry.getKey(), file, true);
      assertFalse(entry.getKey().getSimpleName(), pluginOnTheFly.isEmpty());

      position(ElpMode.ELP);
      assertEmpty(entry.getKey().getSimpleName() + " on the fly", problems(entry.getKey(), file, true));
      assertEmpty(entry.getKey().getSimpleName() + " in batch", problems(entry.getKey(), file, false));
    }
  }

  /** Without a running server the plugin keeps reporting on the fly in the ELP position. */
  public void testSilentWhileTheServerIsNotRunning() throws Exception {
    myClient.setState(com.intellij.platform.lsp.api.LspServerState.ShutdownUnexpectedly);
    assertEmpty(problems(ErlangUnusedFunctionInspection.class, "src/shop_defects.erl", true));
  }

  @NotNull
  private List<TextRange> problems(@NotNull Class<? extends ErlangInspectionBase> inspection, @NotNull String file, boolean onTheFly)
    throws Exception {
    PsiFile psiFile = psiFile(file);
    LocalInspectionToolWrapper wrapper = new LocalInspectionToolWrapper(inspection.getDeclaredConstructor().newInstance());
    DaemonProgressIndicator indicator = new DaemonProgressIndicator();
    Map<LocalInspectionToolWrapper, List<ProblemDescriptor>> problems = ProgressManager.getInstance().runProcess(
      () -> InspectionEngine.inspectEx(List.of(wrapper), psiFile, psiFile.getTextRange(), psiFile.getTextRange(), onTheFly, false, true,
                                       indicator, (tool, descriptor) -> true), indicator);
    List<TextRange> ranges = new ArrayList<>();
    for (List<ProblemDescriptor> descriptors : problems.values()) {
      for (ProblemDescriptor descriptor : descriptors) {
        TextRange range = ((ProblemDescriptorBase)descriptor).getTextRange();
        if (range != null) ranges.add(range);
      }
    }
    ranges.sort((a, b) -> a.getStartOffset() != b.getStartOffset() ? a.getStartOffset() - b.getStartOffset() : a.getEndOffset() - b.getEndOffset());
    return ranges;
  }

  @NotNull
  private List<Diagnostic> elpDiagnostics(@NotNull Map<String, List<Diagnostic>> diagnostics, @NotNull String defect,
                                          @NotNull List<String> codes) {
    String file = fileOf(defect);
    int offset = offsetOf(defect);
    List<Diagnostic> result = new ArrayList<>();
    for (Diagnostic diagnostic : diagnostics.getOrDefault(file, List.of())) {
      if (diagnostic.getCode() == null || !codes.contains(diagnostic.getCode().getLeft())) continue;
      if (range(file, diagnostic.getRange()).containsOffset(offset)) result.add(diagnostic);
    }
    return result;
  }

  @NotNull
  private TextRange range(@NotNull String file, @NotNull Range range) {
    Document document = document(file(file));
    return new TextRange(document.getLineStartOffset(range.getStart().getLine()) + range.getStart().getCharacter(),
                         document.getLineStartOffset(range.getEnd().getLine()) + range.getEnd().getCharacter());
  }

  private int line(@NotNull String file, int offset) {
    return document(file(file)).getLineNumber(offset);
  }

  @NotNull
  private static String fileOf(@NotNull String defect) {
    return defect.substring(0, defect.indexOf(':'));
  }

  private int offsetOf(@NotNull String defect) {
    String needle = defect.substring(defect.indexOf(':') + 1);
    String text = document(file(fileOf(defect))).getText();
    int start = text.indexOf(needle.replace("|", ""));
    assertTrue(defect, start >= 0);
    return start + needle.indexOf('|');
  }
}
