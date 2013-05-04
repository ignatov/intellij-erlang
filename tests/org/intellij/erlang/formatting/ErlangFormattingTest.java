/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import junit.framework.Assert;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;

import java.io.File;

public class ErlangFormattingTest extends LightCodeInsightFixtureTestCase {
  public static final boolean OVERRIDE_TEST_DATA = false;
  private CodeStyleSettings myTemporarySettings;

  public void doTest() throws Exception { doTest(true); }
  public void doEnterTest() throws Exception { doTest(false); }

  public void doTest(boolean format) throws Exception {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    if (format) {
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        @Override
        public void run() {
          CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile());
        }
      });
    }
    else {
      myFixture.type('\n');
    }

    String after = String.format("%s-after.erl", testName);
    if (OVERRIDE_TEST_DATA) {
      FileUtil.writeToFile(new File(myFixture.getTestDataPath() + "/" + after), myFixture.getFile().getText());
    }
    myFixture.checkResultByFile(after);
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

  public void testAligned() throws Exception {
    getErlangSettings().ALIGN_MULTILINE_BLOCK = true;
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

  public void testIf1() throws Exception { doEnterTest(); }
  public void testIf2() throws Exception { doEnterTest(); }
  public void testIf3() throws Exception { doEnterTest(); }

  public void testTry1() throws Exception { doEnterTest(); }
  public void testTry2() throws Exception { doEnterTest(); }
  public void testTry3() throws Exception { doEnterTest(); }
  public void testTry4() throws Exception { doEnterTest(); }
  public void testTry5() throws Exception { doEnterTest(); }

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
    super.setUp();
    setTestStyleSettings();
  }

  @Override
  public void tearDown() throws Exception {
    restoreStyleSettings();
    super.tearDown();
  }

  private void setTestStyleSettings() {
    CodeStyleSettingsManager settingsManager = CodeStyleSettingsManager.getInstance(getProject());
    CodeStyleSettings currSettings = settingsManager.getCurrentSettings();
    Assert.assertNotNull(currSettings);
    myTemporarySettings = currSettings.clone();
    CodeStyleSettings.IndentOptions indentOptions = myTemporarySettings.getIndentOptions(ErlangFileType.MODULE);
    Assert.assertNotNull(indentOptions);
    settingsManager.setTemporarySettings(myTemporarySettings);
  }

  private void restoreStyleSettings() {
    CodeStyleSettingsManager.getInstance(getProject()).dropTemporarySettings();
  }
}