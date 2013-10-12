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

import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;

public class ErlangAutoIndentTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public static final boolean OVERRIDE_TEST_DATA = false;

  @Override
  protected String getTestDataPath() {
    return "testData/auto-indent/";
  }

  public void doTest() throws Exception {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    myFixture.type("\n");

    String after = String.format("%s-after.erl", testName);
    if (OVERRIDE_TEST_DATA) {
      FileUtil.writeToFile(new File(myFixture.getTestDataPath() + "/" + after), myFixture.getFile().getText());
    }
    myFixture.checkResultByFile(after);
  }

  public void testFunctionClause()              throws Exception { doTest(); }
  public void testAfterExpression()             throws Exception { doTest(); }
  public void testAfterIf()                     throws Exception { doTest(); }
  public void testAfterIf2()                    throws Exception { doTest(); }
  public void test337()                         throws Exception { doTest(); }
  public void testIfBeforeIf()                  throws Exception { doTest(); }
  public void testNoReformatAfterInsertedText() throws Exception { doTest(); }
}