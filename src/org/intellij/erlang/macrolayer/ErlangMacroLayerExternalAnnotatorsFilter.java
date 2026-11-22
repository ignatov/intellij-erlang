package org.intellij.erlang.macrolayer;

import com.intellij.lang.ExternalAnnotatorsFilter;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

/**
 * The expansion-error pass is left out for a file where it has nothing to check, so that the
 * platform creates no external-tool pass for it. That pass queues its annotate step on a merging queue with a delay
 * of 300 ms (ExternalAnnotatorManager of 262) even when every annotator collected nothing, and the daemon ends only
 * after it: in {@code erl_types.erl}, which needs no copy, the end of highlighting came about 300-500 ms later than on
 * master in the IDE. Master's only other Erlang external annotator (dialyzer) is paired with an inspection that is off
 * by default, so master has no such pass.
 */
public final class ErlangMacroLayerExternalAnnotatorsFilter implements ExternalAnnotatorsFilter {
  @Override
  public boolean isProhibited(ExternalAnnotator annotator, PsiFile file) {
    return annotator instanceof ErlangMacroLayerExpansionErrorAnnotator && !ErlangMacroLayerExpansionErrors.mayHaveProblems(file);
  }
}
