package org.intellij.erlang.parser;

import com.intellij.testFramework.ParsingTestCase;
import org.intellij.erlang.ErlangParserDefinition;

/**
 * @author ignatov
 */
public class ErlangParserTest extends ParsingTestCase {
  public ErlangParserTest() {
    super("parser", "erl", new ErlangParserDefinition());
  }

  @Override
  protected String getTestDataPath() {
    return "testData";
  }

  @Override
  protected boolean skipSpaces() {
    return true;
  }

  public void testHelloWorld()        { doTest(true); }
  public void testExport()            { doTest(true); }
  public void testH()                 { doTest(true); }
  public void testMnesia()            { doTest(true); }
  public void testIsDigits()          { doTest(true); }
  public void testDialyzerDataflow()  { doTest(true); }
  public void testTest()              { doTest(true); }
  public void testRecords()           { doTest(true); }
  public void testDialyzerClParse()   { doTest(true); }
  public void testMp4Mux()            { doTest(true); }
  public void testComments()          { doTest(true); }
  public void testMultiline()         { doTest(true); }
  public void testRecordExpression()  { doTest(true); }

  @Override
  protected void doTest(boolean checkResult) {
    OVERWRITE_TESTDATA = true;

    super.doTest(checkResult);
    assertFalse(
      "PsiFile contains error elements",
      toParseTreeText(myFile, skipSpaces(), includeRanges()).contains("PsiErrorElement")
    );
  }
}

