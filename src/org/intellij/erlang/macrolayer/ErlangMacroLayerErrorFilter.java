package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.highlighting.HighlightErrorFilter;
import com.intellij.psi.PsiErrorElement;
import org.jetbrains.annotations.NotNull;

/** Spike 3: hides a syntax error of a form whose expanded copy parses without errors. */
public class ErlangMacroLayerErrorFilter extends HighlightErrorFilter {
  @Override
  public boolean shouldHighlightErrorElement(@NotNull PsiErrorElement element) {
    return !ErlangMacroLayer.isErrorExplainedByExpansion(element) && !ErlangMacroLayerExpansionErrors.isReplacedByExpansionError(element);
  }
}
