/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.formatting;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;

public class ErlangFormattingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final boolean OVERRIDE_TEST_DATA = false;

  private static final String PARASITE_INFIX = "Parasite";
  private static final String INDENT_RELATIVE_OFF_SUFFIX = "__IR_OFF";

  private CodeStyleSettings myTemporarySettings;

  private void doTest() throws Exception { doTest(true);  }
  private void doEnterTest() throws Exception { doTest(false); }

  private void doTest(boolean format) throws Exception {
    boolean parasite = isParasite();
    String appendix = parasite ? "\nfoo() -> ok." : "";

    String inputFile = getInputFileName();
    String inputText = FileUtil.loadFile(new File(getTestDataPath() + inputFile)) + appendix;
    myFixture.configureByText(inputFile, inputText);

    if (format) {
      WriteCommandAction.runWriteCommandAction(myFixture.getProject(), () -> {
        CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile());
      });
    }
    else {
      myFixture.type('\n');
    }

    File outputFile = new File(myFixture.getTestDataPath() + "/" + getExpectedOutputFileName());
    if (!outputFile.exists()) {
      FileUtil.writeToFile(outputFile, "");
      //noinspection UseOfSystemOutOrSystemErr
      System.err.println("Output file " + outputFile.getPath() + " doesn't exist. It was created.");
    }
    String expectedResultText = FileUtil.loadFile(outputFile, true) + appendix;

    //noinspection PointlessBooleanExpression,ConstantConditions
    if (OVERRIDE_TEST_DATA && !parasite) {
      String resultText = myFixture.getFile().getText();
      if (expectedResultText.contains("<caret>")) {
        LogicalPosition caretPosition = myFixture.getEditor().getCaretModel().getPrimaryCaret().getLogicalPosition();
        int offset = StringUtil.lineColToOffset(resultText, caretPosition.line, caretPosition.column);
        assertTrue(offset >= 0);
        resultText = TextRange.from(offset, 0).replace(resultText, "<caret>");
      }
      FileUtil.writeToFile(outputFile, resultText);
    }
    else {
      myFixture.checkResult(expectedResultText);
    }
  }

  private String getInputFileName() {
    return StringUtil.trimEnd(getTestName(true).replace(PARASITE_INFIX, ""), INDENT_RELATIVE_OFF_SUFFIX) + ".erl";
  }

  private String getExpectedOutputFileName() {
    return getTestName(true).replace(PARASITE_INFIX, "") + "-after.erl";
  }

  private boolean isParasite() {
    return StringUtil.contains(getTestName(true), PARASITE_INFIX);
  }

  public void test48()     throws Exception { doTest(); }
  public void test52()     throws Exception { doTest(); }
  public void test53()     throws Exception { doTest(); }
  public void test67()     throws Exception { doTest(); }
  public void test71()     throws Exception { doTest(); }
  public void test75()     throws Exception { doTest(); }
  public void test82()     throws Exception { doTest(); }
  public void test95()     throws Exception { doTest(); }
  public void test116()    throws Exception { doTest(); }
  public void test118()    throws Exception { doTest(); }
  public void test136()    throws Exception { doTest(); }
  public void test137()    throws Exception { doTest(); }
  public void test141()    throws Exception { doTest(); }
  public void test125()    throws Exception { doTest(); }
  public void test171()    throws Exception { doTest(); }
  public void test191()    throws Exception { doTest(); }
  public void testSimple() throws Exception { doTest(); }
  public void testCaseEx() throws Exception { doTest(); }
  public void test288()    throws Exception { doTest(); }
  public void test299()    throws Exception { doTest(); }
  public void test305()    throws Exception { doTest(); }
  public void test350()    throws Exception { doTest(); }
  public void test351()    throws Exception { doTest(); }
  public void test222()          throws Exception { doTest(); }
  public void test222__IR_OFF()  throws Exception { doTest(); }
  public void test273()    throws Exception { getErlangSettings().ALIGN_GUARDS = true; doTest(); }
  public void test379()    throws Exception { setUpCommaFirst(); doTest(); }
  public void test433()    throws Exception { getErlangSettings().ALIGN_FUN_CLAUSES = true; doTest(); }
  public void test434()    throws Exception { alignBlocks(); doTest(); }
  public void test451()    throws Exception { getErlangSettings().ALIGN_RECORD_FIELD_ASSIGNMENTS = true; setUpCommaFirst(); doTest(); }
  public void test444()    throws Exception { alignBlocks(); doTest(); }
  public void test478()    throws Exception { doTest(); }

  public void test292() throws Exception {
    ErlangCodeStyleSettings erlangSettings = getErlangSettings();
    erlangSettings.ALIGN_MULTILINE_BLOCK = true;
    erlangSettings.NEW_LINE_BEFORE_COMMA = true;
    erlangSettings.SPACE_AROUND_OR_IN_LISTS = false;
    doTest();
  }

  public void testAligned() throws Exception {
    alignBlocks();
    doTest();
  }

  public void testFunctionClausesAligned() throws Exception {
    getErlangSettings().ALIGN_FUNCTION_CLAUSES = true;
    doTest();
  }

  public void testKeepCommentAtTheFirstLine() throws Exception {
    getCommonSettings().KEEP_FIRST_COLUMN_COMMENT = true;
    doTest();
  }

  public void testNotKeepCommentAtTheFirstLine() throws Exception {
    getCommonSettings().KEEP_FIRST_COLUMN_COMMENT = false;
    doTest();
  }

  public void testNewLineAfterArrow1() throws Exception {
    getErlangSettings().NEW_LINE_AFTER_ARROW = ErlangCodeStyleSettings.NewLineAfterArrow.FORCE;
    doTest();
  }
  public void testNewLineAfterArrow2() throws Exception {
    getErlangSettings().NEW_LINE_AFTER_ARROW = ErlangCodeStyleSettings.NewLineAfterArrow.FORCE_EXCEPT_ONE_LINE_CLAUSES;
    doTest();
  }

  public void testUniformBinaryExpressionsStyle() throws Exception {
    getErlangSettings().UNIFORM_BINARY_EXPRESSIONS = true;
    alignBlocks();
    doTest();
  }

  public void testFunctionClause() throws Exception { doEnterTest(); }


  public void testIf1() throws Exception { doEnterTest(); }
  public void testIf2() throws Exception { doEnterTest(); }
  public void testIf3() throws Exception { doEnterTest(); }

  public void testIfParasite1() throws Exception { doEnterTest(); }
  public void testIfParasite2() throws Exception { doEnterTest(); }
  public void testIfParasite3() throws Exception { doEnterTest(); }

  public void testBindIf()                    throws Exception { doEnterTest(); }
  public void testBindIfFirstClause()         throws Exception { doEnterTest(); }
  public void testBindIfLastClause()          throws Exception { doEnterTest(); }

  public void testBindIf__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindIfFirstClause__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindIfLastClause__IR_OFF()  throws Exception { doEnterTest(); }

  public void testBindIfParasite()            throws Exception { doEnterTest(); }
  public void testBindIfFirstClauseParasite() throws Exception { doEnterTest(); }
  public void testBindIfLastClauseParasite()  throws Exception { doEnterTest(); }

  public void testBindIfParasite__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindIfFirstClauseParasite__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindIfLastClauseParasite__IR_OFF()  throws Exception { doEnterTest(); }


  public void testTry1() throws Exception { doEnterTest(); }
  public void testTry2() throws Exception { doEnterTest(); }
  public void testTry3() throws Exception { doEnterTest(); }
  public void testTry4() throws Exception { doEnterTest(); }
  public void testTry5() throws Exception { doEnterTest(); }

  public void testTryParasite1() throws Exception { doEnterTest(); }
  public void testTryParasite2() throws Exception { doEnterTest(); }
  public void testTryParasite3() throws Exception { doEnterTest(); }
  public void testTryParasite4() throws Exception { doEnterTest(); }
  public void testTryParasite5() throws Exception { doEnterTest(); }

  public void testBindTry()                 throws Exception { doEnterTest(); }
  public void testBindTryExpr()             throws Exception { doEnterTest(); }
  public void testBindTryCatch()            throws Exception { doEnterTest(); }
  public void testBindTryFirstCatchClause() throws Exception { doEnterTest(); }
  public void testBindTryLastCatchClause()  throws Exception { doEnterTest(); }

  public void testBindTry__IR_OFF()                 throws Exception { doEnterTest(); }
  public void testBindTryExpr__IR_OFF()             throws Exception { doEnterTest(); }
  public void testBindTryCatch__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindTryFirstCatchClause__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindTryLastCatchClause__IR_OFF()  throws Exception { doEnterTest(); }

  public void testBindTryParasite()                 throws Exception { doEnterTest(); }
  public void testBindTryExprParasite()             throws Exception { doEnterTest(); }
  public void testBindTryCatchParasite()            throws Exception { doEnterTest(); }
  public void testBindTryFirstCatchClauseParasite() throws Exception { doEnterTest(); }
  public void testBindTryLastCatchClauseParasite()  throws Exception { doEnterTest(); }

  public void testBindTryParasite__IR_OFF()                 throws Exception { doEnterTest(); }
  public void testBindTryExprParasite__IR_OFF()             throws Exception { doEnterTest(); }
  public void testBindTryCatchParasite__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindTryFirstCatchClauseParasite__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindTryLastCatchClauseParasite__IR_OFF()  throws Exception { doEnterTest(); }


  public void testCase1() throws Exception { doEnterTest(); }
  public void testCase2() throws Exception { doEnterTest(); }
  public void testCase3() throws Exception { doEnterTest(); }
  public void testCase4() throws Exception { doEnterTest(); }

  public void testCaseParasite1() throws Exception { doEnterTest(); }
  public void testCaseParasite2() throws Exception { doEnterTest(); }
  public void testCaseParasite3() throws Exception { doEnterTest(); }
  public void testCaseParasite4() throws Exception { doEnterTest(); }

  public void testBindCase()              throws Exception { doEnterTest(); }
  public void testBindCaseExpr()          throws Exception { doEnterTest(); }
  public void testBindCaseOf()            throws Exception { doEnterTest(); }
  public void testBindCaseOfFirstClause() throws Exception { doEnterTest(); }
  public void testBindCaseOfLastClause()  throws Exception { doEnterTest(); }

  public void testBindCase__IR_OFF()              throws Exception { doEnterTest(); }
  public void testBindCaseExpr__IR_OFF()          throws Exception { doEnterTest(); }
  public void testBindCaseOf__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindCaseOfFirstClause__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindCaseOfLastClause__IR_OFF()  throws Exception { doEnterTest(); }

  public void testBindCaseParasite()              throws Exception { doEnterTest(); }
  public void testBindCaseExprParasite()          throws Exception { doEnterTest(); }
  public void testBindCaseOfParasite()            throws Exception { doEnterTest(); }
  public void testBindCaseOfFirstClauseParasite() throws Exception { doEnterTest(); }
  public void testBindCaseOfLastClauseParasite()  throws Exception { doEnterTest(); }

  public void testBindCaseParasite__IR_OFF()              throws Exception { doEnterTest(); }
  public void testBindCaseExprParasite__IR_OFF()          throws Exception { doEnterTest(); }
  public void testBindCaseOfParasite__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindCaseOfFirstClauseParasite__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindCaseOfLastClauseParasite__IR_OFF()  throws Exception { doEnterTest(); }


  public void testReceive() throws Exception { doTest(); }
  public void testReceive1() throws Exception { doEnterTest(); }
  public void testReceive2() throws Exception { doEnterTest(); }
  public void testReceive3() throws Exception { doEnterTest(); }
  public void testReceive4() throws Exception { doEnterTest(); }
  public void testReceive5() throws Exception { doEnterTest(); }
  public void testReceive6() throws Exception { doEnterTest(); }
  public void testReceive7() throws Exception { doEnterTest(); }

  public void testReceiveParasite1() throws Exception { doEnterTest(); }
  public void testReceiveParasite2() throws Exception { doEnterTest(); }
  public void testReceiveParasite3() throws Exception { doEnterTest(); }
  public void testReceiveParasite4() throws Exception { doEnterTest(); }
  public void testReceiveParasite5() throws Exception { doEnterTest(); }
  public void testReceiveParasite6() throws Exception { doEnterTest(); }
  public void testReceiveParasite7() throws Exception { doEnterTest(); }

  public void testBindReceive()            throws Exception { doEnterTest(); }
  public void testBindReceiveFirstClause() throws Exception { doEnterTest(); }
  public void testBindReceiveLastClause()  throws Exception { doEnterTest(); }
  public void testBindReceiveAfter()       throws Exception { doEnterTest(); }
  public void testBindReceiveAfterClause() throws Exception { doEnterTest(); }

  public void testBindReceive__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindReceiveFirstClause__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindReceiveLastClause__IR_OFF()  throws Exception { doEnterTest(); }
  public void testBindReceiveAfter__IR_OFF()       throws Exception { doEnterTest(); }
  public void testBindReceiveAfterClause__IR_OFF() throws Exception { doEnterTest(); }

  public void testBindReceiveParasite()            throws Exception { doEnterTest(); }
  public void testBindReceiveFirstClauseParasite() throws Exception { doEnterTest(); }
  public void testBindReceiveLastClauseParasite()  throws Exception { doEnterTest(); }
  public void testBindReceiveAfterParasite()       throws Exception { doEnterTest(); }
  public void testBindReceiveAfterClauseParasite() throws Exception { doEnterTest(); }

  public void testBindReceiveParasite__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindReceiveFirstClauseParasite__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindReceiveLastClauseParasite__IR_OFF()  throws Exception { doEnterTest(); }
  public void testBindReceiveAfterParasite__IR_OFF()       throws Exception { doEnterTest(); }
  public void testBindReceiveAfterClauseParasite__IR_OFF() throws Exception { doEnterTest(); }


  public void testBegin1() throws Exception { doEnterTest(); }
  public void testBegin2() throws Exception { doEnterTest(); }
  public void testBegin3() throws Exception { doEnterTest(); }

  public void testBeginParasite1() throws Exception { doEnterTest(); }
  public void testBeginParasite2() throws Exception { doEnterTest(); }
  public void testBeginParasite3() throws Exception { doEnterTest(); }

  public void testBindBegin()     throws Exception { doEnterTest(); }
  public void testBindBeginExpr() throws Exception { doEnterTest(); }

  public void testBindBegin__IR_OFF()     throws Exception { doEnterTest(); }
  public void testBindBeginExpr__IR_OFF() throws Exception { doEnterTest(); }

  public void testBindBeginParasite()     throws Exception { doEnterTest(); }
  public void testBindBeginExprParasite() throws Exception { doEnterTest(); }

  public void testBindBeginParasite__IR_OFF()     throws Exception { doEnterTest(); }
  public void testBindBeginExprParasite__IR_OFF() throws Exception { doEnterTest(); }


  public void testRecordFields1()                             throws Exception { doEnterTest(); }
  public void testRecordFields2()                             throws Exception { doEnterTest(); }
  public void testRecordExpressionEmpty()                     throws Exception { doEnterTest(); }
  public void testRecordExpressionBeforeFieldAssignment()     throws Exception { doEnterTest(); }
  public void testRecordExpressionAfterFirstFieldAssignment() throws Exception { doEnterTest(); }
  public void testRecordExpressionAfterLastFieldAssignment()  throws Exception { doEnterTest(); }


  public void testMapTupleEmpty()            throws Exception { doEnterTest(); }
  public void testMapTupleBeforeFirstEntry() throws Exception { doEnterTest(); }
  public void testMapTupleAfterFirstEntry()  throws Exception { doEnterTest(); }
  public void testMapTupleAfterLastEntry()   throws Exception { doEnterTest(); }


  public void testFunExpression() throws Exception { doTest(); }
  public void testFunExpression1() throws Exception { doEnterTest(); }
  public void testFunExpression2() throws Exception { doEnterTest(); }
  public void testFunExpression3() throws Exception { doEnterTest(); }
  public void testFunExpression4() throws Exception { doEnterTest(); }

  public void testFunExpressionParasite1() throws Exception { doEnterTest(); }
  public void testFunExpressionParasite2() throws Exception { doEnterTest(); }
  public void testFunExpressionParasite3() throws Exception { doEnterTest(); }
  public void testFunExpressionParasite4() throws Exception { doEnterTest(); }

  public void testBindFunExpression()            throws Exception { doEnterTest(); }
  public void testBindFunExpressionFirstClause() throws Exception { doEnterTest(); }
  public void testBindFunExpressionLastClause()  throws Exception { doEnterTest(); }

  public void testBindFunExpression__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindFunExpressionFirstClause__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindFunExpressionLastClause__IR_OFF()  throws Exception { doEnterTest(); }

  public void testBindFunExpressionParasite()            throws Exception { doEnterTest(); }
  public void testBindFunExpressionFirstClauseParasite() throws Exception { doEnterTest(); }
  public void testBindFunExpressionLastClauseParasite()  throws Exception { doEnterTest(); }

  public void testBindFunExpressionParasite__IR_OFF()            throws Exception { doEnterTest(); }
  public void testBindFunExpressionFirstClauseParasite__IR_OFF() throws Exception { doEnterTest(); }
  public void testBindFunExpressionLastClauseParasite__IR_OFF()  throws Exception { doEnterTest(); }


  public void testFunCallNoArgs()         throws Exception{ doEnterTest(); }
  public void testFunCallBeforeFirstArg() throws Exception{ doEnterTest(); }
  public void testFunCallAfterComma()     throws Exception{ doEnterTest(); }


  public void testFunArgsEmpty()       throws Exception { doEnterTest(); }
  public void testFunArgsBeforeFirst() throws Exception { doEnterTest(); }
  public void testFunArgsAfterFirst()  throws Exception { doEnterTest(); }
  public void testFunArgsAfterLast()   throws Exception { doEnterTest(); }


  public void testListComprehensionBeforeExpression() throws Exception { doEnterTest(); }
  public void testListComprehensionAfterExpression()  throws Exception { doEnterTest(); }
  public void testListComprehensionAfterOrOr()        throws Exception { doEnterTest(); }
  public void testListComprehensionAfterFirstExpr()   throws Exception { doEnterTest(); }
  public void testListComprehensionAfterLastExpr()    throws Exception { doEnterTest(); }

  public void testBinaryComprehensionBeforeExpression() throws Exception { doEnterTest(); }
  public void testBinaryComprehensionAfterExpression()  throws Exception { doEnterTest(); }
  public void testBinaryComprehensionAfterOrOr()        throws Exception { doEnterTest(); }
  public void testBinaryComprehensionAfterFirstExpr()   throws Exception { doEnterTest(); }
  public void testBinaryComprehensionAfterLastExpr()    throws Exception { doEnterTest(); }

  public void testMapComprehensionBeforeExpression() throws Exception { doEnterTest(); }
  public void testMapComprehensionAfterExpression()  throws Exception { doEnterTest(); }
  public void testMapComprehensionAfterOrOr()        throws Exception { doEnterTest(); }
  public void testMapComprehensionAfterFirstExpr()   throws Exception { doEnterTest(); }
  public void testMapComprehensionAfterLastExpr()    throws Exception { doEnterTest(); }
  
  public void testListAlignOnEnter()                 throws Exception { alignBlocks(); doEnterTest(); }

  public void testCommaFirstEnterRecords() throws Exception { setUpCommaFirst(); doEnterTest(); }

  public void testCommaFirstEnter()        throws Exception { setUpCommaFirst(); doEnterTest(); }
  public void testCommaFirstEnter2()       throws Exception { setUpCommaFirst(); doEnterTest(); }
  public void testNamedFunExpr()   throws Exception { doTest(); }

  public void testComprehensions() throws Exception { doTest(); }

  private void setUpCommaFirst() {
    getErlangSettings().NEW_LINE_BEFORE_COMMA = true;
    alignBlocks();
  }

  private void alignBlocks() { getErlangSettings().ALIGN_MULTILINE_BLOCK = true; }

  private ErlangCodeStyleSettings getErlangSettings() {
    return myTemporarySettings.getCustomSettings(ErlangCodeStyleSettings.class);
  }

  private CommonCodeStyleSettings getCommonSettings() {
    return myTemporarySettings.getCommonSettings(ErlangLanguage.INSTANCE);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/formatter/";
  }

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    setTestStyleSettings();
    getErlangSettings().INDENT_RELATIVE = !getTestName(true).endsWith(INDENT_RELATIVE_OFF_SUFFIX);
  }

  @Override
  public void tearDown() throws Exception {
    restoreStyleSettings();
    super.tearDown();
  }

  private void setTestStyleSettings() {
    CodeStyleSettingsManager settingsManager = CodeStyleSettingsManager.getInstance(getProject());
    CodeStyleSettings currSettings = settingsManager.getCurrentSettings();
    assertNotNull(currSettings);
    myTemporarySettings = currSettings.clone();
    CodeStyleSettings.IndentOptions indentOptions = myTemporarySettings.getIndentOptions(ErlangFileType.MODULE);
    assertNotNull(indentOptions);
    settingsManager.setTemporarySettings(myTemporarySettings);
  }

  private void restoreStyleSettings() {
    CodeStyleSettingsManager.getInstance(getProject()).dropTemporarySettings();
  }
}