package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.find.FindManager;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesManager;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usages.UsageInfo2UsageAdapter;
import com.intellij.usages.UsageSearcher;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Set;
import java.util.TreeSet;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * #1484: Show Usages with the caret on the macro argument that names a function ({@code big_order} in
 * {@code ?TEST(big_order, ...)}) finds the same usages as from a call of the function: the export and the call.
 */
public class ErlangMacroLayerShowUsagesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String TEXT = """
    -module(shop_tests).
    -export([all/0, big_order/0]).
    -define(TEST(Name, Expr), Name() -> Expr).
    ?TEST(big_order, too_big = orders:check(500)).
    all() ->
      big_order().
    """;

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

  /**
   * Show Usages as the IDE runs it: the target at the caret (found in the background, as the data rules do), its
   * find usages handler and the usage searcher. The lines of the usages found.
   */
  private String showUsages(PsiFile f, String marker) throws Exception {
    Editor editor = myFixture.getEditor();
    int caret = f.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    editor.getCaretModel().moveToOffset(caret);
    PsiElement target = inBackgroundWithIndicator(
      () -> TargetElementUtil.getInstance().findTargetElement(editor, TargetElementUtil.getInstance().getAllAccepted(), caret));
    assertNotNull("no target at " + marker, target);
    FindUsagesManager manager = ((FindManagerImpl)FindManager.getInstance(getProject())).getFindUsagesManager();
    FindUsagesHandler handler = manager.getFindUsagesHandler(target, FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    assertNotNull("no find usages handler for " + target + " at " + marker, handler);
    FindUsagesOptions options = handler.getFindUsagesOptions(null);
    UsageSearcher searcher = FindUsagesManager.createUsageSearcher(handler, handler.getPrimaryElements(), handler.getSecondaryElements(), options);
    Set<Integer> lines = inBackgroundWithIndicator(() -> {
      Set<Integer> r = new TreeSet<>();
      searcher.generate(u -> {
        if (u instanceof UsageInfo2UsageAdapter a && a.getElement() != null) {
          r.add(StringUtil.offsetToLineNumber(f.getText(), a.getUsageInfo().getNavigationOffset()) + 1);
        }
        return true;
      });
      return r;
    });
    String result = target.getClass().getSimpleName() + " " + lines;
    System.out.println("SHOW-USAGES at " + marker + ": " + result);
    return result;
  }

  public void testFromTheMacroArgumentThatNamesAFunction() throws Exception {
    PsiFile f = myFixture.addFileToProject("su/shop_tests.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("from the call", "LeafPsiElement [2, 6]", showUsages(f, "b|ig_order()."));
    assertEquals("from the macro argument", "LeafPsiElement [2, 6]", showUsages(f, "?TEST(b|ig_order"));
  }

  public void testFromTheArgumentOfAOneArgumentMacro() throws Exception {
    PsiFile f = myFixture.addFileToProject("su/mk.erl", """
      -module(mk).
      -export([made/0, d/0]).
      -define(MK(N), N() -> ok).
      ?MK(made).
      d() -> made().
      """);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("from the call", "LeafPsiElement [2, 5]", showUsages(f, "d() -> m|ade()"));
    assertEquals("from the macro argument", "LeafPsiElement [2, 5]", showUsages(f, "?MK(m|ade"));
  }

  public void testPlainAtomKeepsTheAtomTarget() throws Exception {
    PsiFile f = myFixture.addFileToProject("su/plain.erl", """
      -module(plain).
      -export([a/0, b/0]).
      a() -> some_atom.
      b() -> some_atom.
      """);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("ErlangQAtomImpl [3, 4]", showUsages(f, "a() -> s|ome_atom"));
  }
}
