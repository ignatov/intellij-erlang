package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesManager;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.find.impl.FindManagerImpl;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageInfo;
import com.intellij.usageView.UsageViewUtil;
import com.intellij.util.CommonProcessors;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5, last fix round: a record made by a same-file macro whose field has a default value. In the -define body,
 * {@code sf = 0} is an assignment expression; the target of {@code sf} is the field name, not that expression, so find
 * usages and rename work and name it as a person would. The expected rename result compiles with erlc 29.1.
 */
public class ErlangMacroLayerFieldDefaultTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String TEXT = """
    -module(p24).
    -export([f/1, h/0]).
    -define(SR, -record(sr, {sf = 0, sg})).
    ?SR.
    f(R) -> {R#sr.sf, R#sr.sg}.
    h() -> #sr{sf = 2}.
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

  private PsiFile open() {
    PsiFile f = myFixture.addFileToProject("p24.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    return f;
  }

  private static PsiElement target(PsiFile f, String marker, int shift) throws Exception {
    int at = f.getText().indexOf(marker) + shift;
    return inBackground(() -> {
      PsiReference ref = f.findReferenceAt(at);
      return ref == null ? null : ref.resolve();
    });
  }

  public void testFindUsagesOfAFieldWithADefaultValue() throws Exception {
    PsiFile f = open();
    PsiElement t = target(f, "#sr{sf = 2}", 4);
    assertNotNull(t);
    assertEquals("sf", inBackground(t::getText));
    String type = inBackground(() -> UsageViewUtil.getType(t));
    assertFalse(type, type.isEmpty() || type.contains("Impl") || type.contains("ssignment"));
    FindUsagesManager manager = ((FindManagerImpl)com.intellij.find.FindManager.getInstance(getProject())).getFindUsagesManager();
    FindUsagesHandler handler = manager.getFindUsagesHandler(t, FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    assertNotNull("no find usages handler for " + t, handler);
    FindUsagesOptions options = handler.getFindUsagesOptions(null);
    List<UsageInfo> found = waitForTask(ApplicationManager.getApplication().executeOnPooledThread(() -> {
      CommonProcessors.CollectProcessor<UsageInfo> p = new CommonProcessors.CollectProcessor<>(Collections.synchronizedList(new ArrayList<>()));
      for (PsiElement e : handler.getPrimaryElements()) handler.processElementUsages(e, p, options);
      return new ArrayList<>(p.getResults());
    }));
    Set<Integer> lines = inBackground(() -> {
      Set<Integer> r = new TreeSet<>();
      for (UsageInfo u : found) {
        if (u.getElement() != null) r.add(StringUtil.offsetToLineNumber(f.getText(), u.getElement().getTextRange().getStartOffset()) + 1);
      }
      return r;
    });
    assertEquals(Set.of(5, 6), lines);
  }

  public void testRenameOfAFieldWithADefaultValue() {
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    PsiFile f = open();
    myFixture.getEditor().getCaretModel().moveToOffset(f.getText().indexOf("#sr{sf = 2}") + 4);
    PsiElement t = TargetElementUtil.findTargetElement(myFixture.getEditor(),
      TargetElementUtil.ELEMENT_NAME_ACCEPTED | TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED);
    assertNotNull(t);
    myFixture.renameElement(t, "sf2");
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals(TEXT.replace("sf", "sf2"), f.getText());
  }
}
