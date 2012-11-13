package org.intellij.erlang.quickfix;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;

import java.util.List;

public class ErlangExportFunctionFixTest extends LightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    myFixture.enableInspections(ErlangUnusedFunctionInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/export/";
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  public void testEmpty() throws Throwable    { doTest("Export function"); }
  public void testWithout() throws Throwable  { doTest("Export function"); }
  public void testCommon() throws Throwable  { doTest("Export function"); }
  public void testDelete() throws Throwable  { doTest("Remove function"); }


  protected void doTest(final String quickFixName) {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    List<IntentionAction> availableIntentions = myFixture.filterAvailableIntentions(quickFixName);
    IntentionAction action = ContainerUtil.getFirstItem(availableIntentions);
    assertNotNull(action);
    myFixture.launchAction(action);
    String after = String.format("%s-after.erl", testName);
    myFixture.checkResultByFile(after);
  }
}
