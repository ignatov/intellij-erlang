package org.intellij.erlang.typing;

import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/**
 * @author savenko
 */
public class ErlangEnterHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  @Override
  protected String getTestDataPath() {
    return "testData/autotemplates/";
  }

  public void testBeginEndSimple() throws Throwable { doTest(); }
  public void testBeginEndNested() throws Throwable { doTest(); }
  public void testBeginEndWithSucceedingExpressions() throws Throwable { doTest(); }
  public void testBeginEndWithSucceedingTryCatchBlock() throws Throwable { doTest(); }
  public void testBeginEndWithSucceedingAfterClause() throws Throwable { doTest(); }

  private void doTest() throws Throwable {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.type('\n');
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }

}
