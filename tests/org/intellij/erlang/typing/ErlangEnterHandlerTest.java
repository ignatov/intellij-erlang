package org.intellij.erlang.typing;

import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangEnterHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  @Override
  protected String getTestDataPath() {
    return "testData/autotemplates/";
  }

  public void testBeginEndSimple() throws Throwable                       { doTest(); }
  public void testBeginEndNested() throws Throwable                       { doTest(); }
  public void testBeginEndWithSucceedingExpressions() throws Throwable    { doTest(); }
  public void testBeginEndWithSucceedingTryCatchBlock() throws Throwable  { doTest(); }
  public void testBeginEndWithSucceedingAfterClause() throws Throwable    { doTest(); }

  public void testCaseOfSimple() throws Throwable                         { doTest(); }
  public void testCaseOfWithSucceedingFunctions() throws Throwable        { doTest(); }

  public void testReceiveSimple() throws Throwable                        { doTest(); }
  public void testReceiveWithSucceedingFunctions() throws Throwable       { doTest(); }

  public void testIfSimple() throws Throwable                             { doTest(); }
  public void testIfWithSucceedingFunctions() throws Throwable            { doTest(); }

  public void testTryCatchSimple() throws Throwable                       { doTest(); }
  public void testTryOfSimple() throws Throwable                          { doTest(); }
  public void testTryOfCatchSimple() throws Throwable                     { doTest(); }

  private void doTest() throws Throwable {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.type('\n');
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }
}
