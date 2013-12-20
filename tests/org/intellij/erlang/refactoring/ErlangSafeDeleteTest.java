package org.intellij.erlang.refactoring;

import com.intellij.psi.PsiElement;
import com.intellij.refactoring.safeDelete.SafeDeleteHandler;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;

public class ErlangSafeDeleteTest extends LightCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  @Override
    protected String getTestDataPath() {
      return "testData/refactoring/delete/";
    }

  private void doTest() {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    PsiElement element = myFixture.getElementAtCaret();
    SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{element}, false);
    myFixture.checkResultByFile(testName + "-after.erl");
  }

  public void testFunction()          throws Exception { doTest(); }
  public void testWithExports()       throws Exception { doTest(); }
}
