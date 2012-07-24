package org.intellij.erlang.highlighting;

import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;

public class ErlangHighlightingTest extends LightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  protected void doTest() {
    myFixture.configureByFile(getTestName(false) + ".erl");
    myFixture.checkHighlighting(true, false, false);
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/";
  }

  public void testHelloWorld()        { doTest(); }
  public void testExport()            { doTest(); }
  public void testH()                 { doTest(); }
  public void testMnesia()            { doTest(); }
  public void testIsDigits()          { doTest(); }
  public void testDialyzerDataflow()  { doTest(); }
  public void testTest()              { doTest(); }
  public void testRecords()           { doTest(); }
  public void testDialyzerClParse()   { doTest(); }
  public void testMp4Mux()            { doTest(); }
  public void testRecord()            { doTest(); }
}
