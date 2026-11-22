package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation;
import com.intellij.codeInsight.navigation.CtrlMouseData;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationOrUsageHandler2;
import com.intellij.ide.util.DeleteNameDescriptionLocation;
import com.intellij.ide.util.DeleteTypeDescriptionLocation;
import com.intellij.lang.documentation.ide.IdeDocumentationTargetProvider;
import com.intellij.lang.findUsages.LanguageFindUsages;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.platform.backend.documentation.DocumentationTarget;
import com.intellij.psi.*;
import com.intellij.refactoring.util.NonCodeSearchDescriptionLocation;
import com.intellij.refactoring.util.RefactoringDescriptionLocation;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * The owner's quick-documentation error, the same class for the branch: every kind of element that the branch can hand
 * to the platform as a target (a macro use, a bare {@code ?NAME} host, a name made by a macro, a header target, an
 * element mapped back from the copy, the file as a reference host) goes through the presentation and description paths
 * of the platform, and quick documentation and ctrl-hover run at the uses that lead to them. A logged error fails.
 */
public class ErlangMacroLayerTargetKindsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final ElementDescriptionLocation[] LOCATIONS = {UsageViewNodeTextLocation.INSTANCE, UsageViewLongNameLocation.INSTANCE,
    UsageViewShortNameLocation.INSTANCE, UsageViewTypeLocation.INSTANCE, HighlightUsagesDescriptionLocation.INSTANCE,
    RefactoringDescriptionLocation.WITH_PARENT, RefactoringDescriptionLocation.WITHOUT_PARENT, DeleteNameDescriptionLocation.INSTANCE,
    DeleteTypeDescriptionLocation.SINGULAR, DeleteTypeDescriptionLocation.PLURAL, NonCodeSearchDescriptionLocation.STRINGS_AND_COMMENTS,
    NonCodeSearchDescriptionLocation.NON_JAVA};

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private static PsiElement resolve(PsiFile f, String marker) throws Exception {
    int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    return inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      if (ref == null) return null;
      if (ref instanceof PsiPolyVariantReference p) {
        ResolveResult[] r = p.multiResolve(false);
        if (r.length > 0) return r[0].getElement();
      }
      return ref.resolve();
    });
  }

  public void testTargetKindsThroughThePlatformsPaths() throws Exception {
    myFixture.addFileToProject("k/h.hrl", "-define(HX, Xh = 5).\n-define(HGEN(N), N() -> gen_ok).\n");
    PsiFile f = myFixture.addFileToProject("k/kinds.erl", """
      -module(kinds).
      -include("h.hrl").
      -export([main/0, k/0, g/0]).
      -define(PAR, ()).
      -define(ONE, 1).
      -define(DEF(N), N() -> ok).
      ?DEF(hello).
      ?HGEN(hfoo).
      main() ->
        foo(),
        foo?PAR,
        io:format("hello").
      foo() -> ok.
      k() -> {hello(), hfoo(), ?ONE}.
      g() -> ?HX, Xh.
      """);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    Map<String, PsiElement> kinds = new LinkedHashMap<>();
    kinds.put("macro use", f.findElementAt(f.getText().indexOf("?ONE")).getParent());
    kinds.put("macro name", f.findElementAt(f.getText().indexOf("?ONE") + 1).getParent());
    kinds.put("bare ?NAME host", com.intellij.psi.util.PsiTreeUtil.getParentOfType(f.findElementAt(f.getText().indexOf("foo?PAR")), ErlangClauseBody.class));
    kinds.put("the file", f);
    kinds.put("define of PAR (bare use target)", resolve(f, "foo?P|AR"));
    kinds.put("foo/0 (token reference target, mapped from the copy)", resolve(f, "f|oo?PAR"));
    kinds.put("hello made by ?DEF (mapped from the copy)", resolve(f, "{h|ello()"));
    kinds.put("hfoo made by a header macro", resolve(f, "h|foo()"));
    kinds.put("header target (Xh of a header body)", resolve(f, "X|h."));
    List<String> problems = new ArrayList<>();
    for (Map.Entry<String, PsiElement> e : kinds.entrySet()) {
      PsiElement el = e.getValue();
      if (el == null) {
        problems.add(e.getKey() + ": no element");
        continue;
      }
      try {
        List<String> seen = new ArrayList<>();
        inBackgroundWithIndicator(() -> {
          if (el instanceof NavigationItem n) {
            ItemPresentation p = n.getPresentation();
            if (p != null) seen.add(p.getPresentableText() + " | " + p.getLocationString());
          }
          for (ElementDescriptionLocation l : LOCATIONS) seen.add(String.valueOf(ElementDescriptionUtil.getElementDescription(el, l)));
          var provider = LanguageFindUsages.INSTANCE.forLanguage(ErlangLanguage.INSTANCE);
          seen.add(provider.getType(el) + " | " + provider.getDescriptiveName(el) + " | " + provider.getNodeText(el, true) + " | " + provider.getNodeText(el, false));
          if (el instanceof PsiNamedElement n) seen.add(String.valueOf(n.getName()));
          seen.add(el.getTextOffset() + " " + el.getNavigationElement().getClass().getSimpleName() + " " + el.getUseScope() + " " + el.getContainingFile());
          return null;
        });
        System.out.println("TARGET-KIND " + e.getKey() + " (" + el.getClass().getSimpleName() + "): " + seen);
      }
      catch (Throwable t) {
        problems.add(e.getKey() + ": " + t);
      }
    }
    // quick documentation and ctrl-hover at the uses that lead to these targets
    for (String marker : List.of("foo?P|AR", "f|oo?PAR", "{h|ello()", "h|foo()", "X|h.", "?O|NE}", "?DEF(h|ello)", "?HGEN(h|foo)")) {
      int at = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
      try {
        myFixture.getEditor().getCaretModel().moveToOffset(at);
        for (DocumentationTarget t : IdeDocumentationTargetProvider.getInstance(getProject()).documentationTargets(myFixture.getEditor(), f, at)) {
          t.computePresentation();
        }
        CtrlMouseData hover = GotoDeclarationOrUsageHandler2.getCtrlMouseData(myFixture.getEditor(), f, at);
        if (hover != null) hover.getHintText();
      }
      catch (Throwable t) {
        problems.add(marker + ": " + t);
      }
    }
    assertEquals(List.of(), problems);
  }
}
