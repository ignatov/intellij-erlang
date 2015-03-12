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

package org.intellij.erlang.rename;

import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangRenameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/rename/";
  }

  public void testUnquotedFunction()  { doTest("bar"); }
  public void testQuotedFunction()    { doTest("bar"); }
  public void testQuoteFunctionName() { doTest("BAR"); }

  public void testUnquotedMacro()     { doTest("bar"); }
  public void testQuotedMacro()       { doTest("bar"); }
  public void testUnderscoreMacro()   { doTest("Foo"); }
  public void testMacrosInAttrs()     { doTest("BAR"); }
  
  public void testMultiTarget()       { doTest("Bar"); }
  public void testMultiTargetMacro()  { doTest("BAR"); }
  
  public void testModuleToQuotedName() { doTest("'moduleToQuotedName-after'", true); }
  public void testUnquotedModule()     { doTest("unquotedModule-after", true); }
  public void testQuotedModule()       { doTest("quotedModule-after", true); }

  private void doTest(String newName) {
    doTest(newName, false);
  }

  private void doTest(String newName, boolean isRenameModuleTest) {
    myFixture.testRename(getTestName(true) + ".erl", getTestName(true) + "-after.erl", newName);
    if (isRenameModuleTest) {
      assertEquals(StringUtil.unquoteString(newName, '\'') + ".erl", myFixture.getFile().getName());
    }
  }
}
