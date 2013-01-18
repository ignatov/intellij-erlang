package org.intellij.erlang.quickfixes;

import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;

public class ErlangExportFunctionFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnusedFunctionInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/export/";
  }

  public void testEmpty()      throws Throwable  { doTest("Export function"); }
  public void testWithout()    throws Throwable  { doTest("Export function"); }
  public void testCommon()     throws Throwable  { doTest("Export function"); }
  public void testDelete()     throws Throwable  { doTest("Remove function"); }
  public void testDeleteSpec() throws Throwable  { doTest("Remove function"); }
}
