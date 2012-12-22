package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;

import java.util.List;

public class ErlangExportFunctionFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myFixture.enableInspections(ErlangUnusedFunctionInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/export/";
  }

  public void testEmpty()   throws Throwable  { doTest("Export function"); }
  public void testWithout() throws Throwable  { doTest("Export function"); }
  public void testCommon()  throws Throwable  { doTest("Export function"); }
  public void testDelete()  throws Throwable  { doTest("Remove function"); }
}
