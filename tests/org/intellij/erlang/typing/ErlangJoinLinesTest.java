package org.intellij.erlang.typing;

import com.intellij.openapi.actionSystem.IdeActions;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangJoinLinesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  @Override
  protected String getTestDataPath() {
    return "testData/join-lines/";
  }

  public void testSinglePercentComments() { doTest(); }
  public void testDoublePercentComments() { doTest(); }

  private void doTest() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.performEditorAction(IdeActions.ACTION_EDITOR_JOIN_LINES);
    myFixture.checkResultByFile(getTestName(true) + "-after.erl", true);
  }
}
