package org.intellij.erlang.macrolayer;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.UseScopeEnlarger;
import com.intellij.psi.search.searches.ReferencesSearch;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * #1699: a reference search for an element of another language must not make the layer's searcher compute the
 * effective search scope, which runs the use scope enlargers of every plugin (Markdown's iterates over files).
 */
public class ErlangMacroLayerReferencesSearcherScopeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testNoEffectiveScopeForNonErlangTarget() {
    ErlangMacroLayerSettings.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    List<PsiElement> fromLayer = new ArrayList<>();
    int[] calls = {0};
    UseScopeEnlarger.EP_NAME.getPoint().registerExtension(new UseScopeEnlarger() {
      @Override
      public @Nullable SearchScope getAdditionalUseScope(@NotNull PsiElement element) {
        calls[0]++;
        for (StackTraceElement frame : new Throwable().getStackTrace()) {
          if (frame.getClassName().equals(ErlangMacroLayerReferencesSearcher.class.getName())) {
            fromLayer.add(element);
            break;
          }
        }
        return null;
      }
    }, getTestRootDisposable());

    PsiFile file = myFixture.configureByText("notes.txt", "hello world");
    ReferencesSearch.search(file).findAll();

    assertTrue("the use scope enlarger was never asked: the check sees nothing", calls[0] > 0);
    assertEmpty("ErlangMacroLayerReferencesSearcher computed the effective scope for a " + file.getLanguage() + " target",
                fromLayer);
  }
}
