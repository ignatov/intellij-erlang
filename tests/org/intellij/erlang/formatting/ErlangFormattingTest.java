/*
 * Copyright 2012 Sergey Ignatov
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
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;

import java.io.File;

public class ErlangFormattingTest extends LightPlatformCodeInsightFixtureTestCase {
  public static final boolean OVERRIDE_TEST_DATA = false;

  public void doTest() throws Exception {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile());
      }
    });

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
  public void testSimple() throws Exception { doTest(); }

  public void testAligned() throws Exception {
    getErlangSettings().ALIGN_MULTILINE_BLOCK = true;
    doTest();
  }

  private ErlangCodeStyleSettings getErlangSettings() {
    return CodeStyleSettingsManager.getSettings(getProject()).getCustomSettings(ErlangCodeStyleSettings.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/formatter/";
  }
}