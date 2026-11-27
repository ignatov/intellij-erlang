package org.intellij.erlang.runconfig;

import com.intellij.codeInsight.daemon.GutterMark;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangRunLineMarkerContributorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/runconfig/runLineMarker/";
  }

  /** #1535: {@code hello().} after {@code setup?PAR,} is a call in start/0, not the head of a function. */
  public void testSplitFormCall() {
    doTest("7: start", "11: setup");
  }

  public void testSplitFormCallMacroLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    doTestFile("splitFormCall", "7: start", "11: setup");
  }

  public void testSplitFormCallGutters() {
    myFixture.configureByFile("splitFormCall.erl");
    Document document = myFixture.getEditor().getDocument();
    List<Integer> lines = new ArrayList<>();
    for (GutterMark mark : myFixture.findAllGutters()) {
      if (mark.getIcon() == AllIcons.RunConfigurations.TestState.Run && mark instanceof LineMarkerInfo.LineMarkerGutterIconRenderer<?> r) {
        lines.add(document.getLineNumber(r.getLineMarkerInfo().startOffset) + 1);
      }
    }
    assertEquals(List.of(7, 11), lines);
  }

  /** Function heads after a form that ends with a period keep their marker. */
  public void testOrdinaryHeads() {
    doTest("6: a", "8: b", "10: c", "12: c", "15: d", "18: e", "20: g");
  }

  /** #1696: a quoted function name gets one marker, on the name and not on its quotes. */
  public void testQuotedName() {
    doTest("4: my-test", "5: plain", "6: other");
  }

  private void doTest(String... expected) {
    doTestFile(getTestName(true), expected);
  }

  private void doTestFile(String name, String... expected) {
    PsiFile file = myFixture.configureByFile(name + ".erl");
    Document document = myFixture.getEditor().getDocument();
    ErlangRunLineMarkerContributor contributor = new ErlangRunLineMarkerContributor();
    List<String> marked = new ArrayList<>();
    for (PsiElement leaf : SyntaxTraverser.psiTraverser(file).filter(LeafPsiElement.class)) {
      if (contributor.getInfo(leaf) != null) {
        marked.add((document.getLineNumber(leaf.getTextOffset()) + 1) + ": " + leaf.getText());
      }
    }
    assertEquals(List.of(expected), marked);
  }
}
