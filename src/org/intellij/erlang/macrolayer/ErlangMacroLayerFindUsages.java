package org.intellij.erlang.macrolayer;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.ReferenceRange;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.UsageSearchContext;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.Processor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Find usages, highlight usages and rename of a name that a macro makes ({@code ?GEN(foo).}, a record in a macro body):
 * the name is a leaf, not a named element, so the platform's search has no word for it; the search here looks for its
 * text and keeps the references that resolve to it. Threading: the search runs in the background; reads are in read
 * actions.
 */
public final class ErlangMacroLayerFindUsages {
  private ErlangMacroLayerFindUsages() {
  }

  /** The find-usages handler of a name made by a macro ({@link ErlangMacroLayer#isNameMadeByMacro}). */
  public static @NotNull FindUsagesHandler handlerForNameMadeByMacro(@NotNull PsiElement element) {
    return new NameMadeByMacroHandler(element);
  }

  /**
   * The macro layer resolves a use of a name made by a macro (such as {@code foo()} after {@code ?GEN(foo).},
   * or {@code #mr{}} after a macro whose body is {@code -record(mr, ...)}) to the atom name leaf in the macro use or
   * in the {@code -define} body. That leaf is not a named element, so the platform's reference search has no word to
   * look for. This handler searches the name and keeps the references that resolve to the leaf.
   */
  private static final class NameMadeByMacroHandler extends FindUsagesHandler {
    NameMadeByMacroHandler(@NotNull PsiElement element) {
      super(element);
    }

    @Override
    public boolean processElementUsages(@NotNull PsiElement element,
                                        @NotNull Processor<? super UsageInfo> processor,
                                        @NotNull FindUsagesOptions options) {
      if (!options.isUsages) return true;
      SearchScope scope = read(() -> options.searchScope.intersectWith(element.getUseScope()));
      return processReferencesToNameMadeByMacro(element, scope, ref -> processor.process(new UsageInfo(ref)));
    }

    /**
     * The identifier highlighting and highlight usages ask this method, not
     * {@link #processElementUsages}; the platform's reference search has no word for the leaf, so it found nothing.
     */
    @Override
    public @NotNull Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
      List<PsiReference> r = new ArrayList<>();
      // the name itself: the platform finds no declaration range for a leaf, so it is given as a reference to itself
      PsiFile file = target.getContainingFile();
      if (file != null && file.getVirtualFile() != null && searchScope.contains(file.getVirtualFile())) {
        r.add(new PsiReferenceBase.Immediate<>(target, TextRange.from(0, target.getTextLength()), target));
      }
      processReferencesToNameMadeByMacro(target, searchScope, ref -> {
        r.add(ref);
        return true;
      });
      return r;
    }
  }

  private static <T> T read(@NotNull Computable<T> computable) {
    return ApplicationManager.getApplication().runReadAction(computable);
  }

  /**
   * The references to a name made by a macro (see {@link ErlangMacroLayer#isNameMadeByMacro}): a search of
   * its text that keeps the references resolving to it; the name itself is not its reference. Also used by rename.
   */
  public static boolean processReferencesToNameMadeByMacro(@NotNull PsiElement element, @NotNull SearchScope scope,
                                                           @NotNull Processor<? super PsiReference> processor) {
    // The copies of the files just typed in, before the answers are read
    read(() -> {
      ErlangMacroLayer.prepareSearch(element, scope);
      return null;
    });
    String name = read(element::getText);
    PsiFile declarationFile = read(element::getContainingFile);
    TextRange declaration = read(element::getTextRange);
    return PsiSearchHelper.getInstance(read(element::getProject)).processElementsWithWord((occurrence, offsetInElement) -> {
      boolean atDeclaration = occurrence.getContainingFile() == declarationFile &&
                              declaration.contains(occurrence.getTextRange().getStartOffset() + offsetInElement);
      if (atDeclaration) return true;
      for (PsiReference ref : occurrence.getReferences()) {
        if (ReferenceRange.containsOffsetInElement(ref, offsetInElement) && ref.isReferenceTo(element) && !processor.process(ref)) {
          return false;
        }
      }
      return true;
    }, scope, name, UsageSearchContext.IN_CODE, true);
  }
}
