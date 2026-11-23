package org.intellij.erlang.runconfig;

import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
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

  private void doTest(String... expected) {
    PsiFile file = myFixture.configureByFile(getTestName(true) + ".erl");
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
