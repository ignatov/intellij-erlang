package org.intellij.erlang.macrolayer;

import com.intellij.openapi.application.QueryExecutorBase;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import org.jetbrains.annotations.NotNull;

/**
 * Round 6: runs first among the reference searchers, on the thread that starts the search, and finds nothing itself.
 * It builds the copies of the files of the search that were just typed in ({@link ErlangMacroLayer#prepareSearch}),
 * before the platform's word search asks {@code isReferenceTo} there, possibly on worker threads.
 */
public final class ErlangMacroLayerReferencesSearcher extends QueryExecutorBase<PsiReference, ReferencesSearch.SearchParameters> {
  public ErlangMacroLayerReferencesSearcher() {
    super(true);
  }

  @Override
  public void processQuery(@NotNull ReferencesSearch.SearchParameters parameters, @NotNull Processor<? super PsiReference> consumer) {
    if (!ErlangMacroLayer.isEnabled()) return;
    ErlangMacroLayer.prepareSearch(parameters.getElementToSearch(), parameters.getEffectiveSearchScope());
  }
}
