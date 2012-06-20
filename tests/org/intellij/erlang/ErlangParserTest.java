package org.intellij.erlang;

import com.intellij.testFramework.ParsingTestCase;

/**
 * @author ignatov
 */
public class ErlangParserTest extends ParsingTestCase {
  public ErlangParserTest() {
    super("parser", "erl", new ErlangParserDefinition());
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
//    addExplicitExtension(LanguageBraceMatching.INSTANCE, myLanguage, new ErlangBraceMatcher()); // todo
  }

  @Override
  protected String getTestDataPath() {
    return "testData";
  }

  @Override
  protected boolean skipSpaces() {
    return true;
  }

  public void testHelloWorld() { doTest(true); }
  public void testExport() { doTest(true); }
  public void testH() { doTest(true); }
  public void testMnesia() { doTest(true); }
  public void testIsDigits() { doTest(true); }
  public void testDialyzerDataflow() { doTest(true); }
  public void testTest() { doTest(true); }

  @Override
  protected void doTest(boolean checkResult) {
//    OVERWRITE_TESTDATA = true;

    super.doTest(checkResult);
    assertFalse(
      "PsiFile contains error elements",
      toParseTreeText(myFile, skipSpaces(), includeRanges()).contains("PsiErrorElement")
    );
  }
}

