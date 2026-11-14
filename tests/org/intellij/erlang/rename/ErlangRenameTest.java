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
import org.intellij.erlang.psi.ErlangFile;
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

  public void testMapKeyInFunHeadBody()           { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromKey()    { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromBody()   { doTest("Key2"); }
  public void testMapKeyInFunHeadGuard()          { doTest("Key2"); }
  public void testMapKeyInNestedFunHeads()        { doTest("Key2"); }
  public void testMapKeyInNestedFunHeadsFromOuter() { doTest("Key2"); }
  
  public void testModuleToQuotedName() { doTest("'moduleToQuotedName-after'", true); }
  public void testUnquotedModule()     { doTest("unquotedModule-after", true); }
  public void testQuotedModule()       { doTest("quotedModule-after", true); }

  public void testFileRenameToErl()          { doFileRenameTest("fileRenameToErlNew.erl"); }
  public void testFileRenameToUpperCaseErl() { doFileRenameTest("fileRenameToUpperCaseErlNew.ERL"); }
  public void testFileRenameToBak()          { doFileRenameTest("fileRenameToBak.erl.bak"); }
  public void testFileRenameToHrl()          { doFileRenameTest("fileRenameToHrlNew.hrl"); }
  public void testFileRenameToSameNameHrl()  { doFileRenameTest("fileRenameToSameNameHrl.hrl"); }

  public void testFileRenameToErlWithReferences() { doFileRenameWithReferencesTest("fileRenameToErlWithReferencesNew.erl"); }
  public void testFileRenameToBakWithReferences() { doFileRenameWithReferencesTest("fileRenameToBakWithReferences.erl.bak"); }

  public void testFileRenameToBakAndBack() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.renameElement(myFixture.getFile(), getTestName(true) + ".erl.bak");
    myFixture.renameElement(myFixture.getFile(), getTestName(true) + ".erl");
    assertEquals(getTestName(true) + ".erl", myFixture.getFile().getName());
    assertInstanceOf(myFixture.getFile(), ErlangFile.class);
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }

  private void doFileRenameTest(String newFileName) {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.renameElement(myFixture.getFile(), newFileName);
    assertEquals(newFileName, myFixture.getFile().getName());
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }

  private void doFileRenameWithReferencesTest(String newFileName) {
    String caller = getTestName(true) + "Caller";
    myFixture.configureByFiles(getTestName(true) + ".erl", caller + ".erl");
    myFixture.renameElement(myFixture.getFile(), newFileName);
    assertEquals(newFileName, myFixture.getFile().getName());
    myFixture.checkResultByFile(caller + ".erl", caller + "-after.erl", false);
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }

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
