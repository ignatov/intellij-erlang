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

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangIncorrectModuleNameInspection;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangRenameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/rename/";
  }

  public void testUnquotedFunction()  { doTest("bar"); }
  public void testQuotedFunction()    { doTest("bar"); }
  public void testQuoteFunctionName() { doTest("BAR"); }

  public void testImportedFunctionInExport() {
    myFixture.configureByFiles("importedFunctionInExport.erl", "importedFunctionInExportUser.erl");
    myFixture.renameElementAtCaret("bar");
    myFixture.checkResultByFile("importedFunctionInExport-after.erl");
    myFixture.checkResultByFile("importedFunctionInExportUser.erl", "importedFunctionInExportUser-after.erl", false);
  }

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

  public void testNamedFunName()                  { doTest("Fact"); }
  public void testNamedFunShadowingArgument()     { doTest("Arg"); }
  public void testAnonymousFunLaterClauseVariable() { doTest("Zz"); }
  public void testCaseHeadVariable()               { doTest("NewVar"); }
  public void testCaseHeadVariableFromUse()        { doTest("NewVar"); }
  public void testCaseHeadRematchesParameter()     { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_PARAMETER); }
  public void testCaseHeadRematchesBoundVariable() { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_BOUND_VARIABLE); }
  public void testCaseHeadVariableInNestedReceive() { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE); }
  public void testCaseHeadVariableRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN); }
  
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

  public void testFileRenameToHrlHasNoIncorrectModuleName() {
    myFixture.configureByText("abc.erl", "-module(abc).\n\nf() -> ok.\n");
    myFixture.renameElement(myFixture.getFile(), "def.hrl");
    assertEquals("def.hrl", myFixture.getFile().getName());
    myFixture.enableInspections(ErlangIncorrectModuleNameInspection.class);
    List<HighlightInfo> infos = myFixture.doHighlighting(HighlightSeverity.WARNING);
    assertEmpty(ContainerUtil.filter(infos, info -> info.getDescription() != null && info.getDescription().startsWith("Module with name")));
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

  private static final String CASE_HEAD_REMATCHES_PARAMETER =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of _ -> Var end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCHES_BOUND_VARIABLE =
    "-module(a).\n-export([foo/0]).\nfoo() -> Var = 1, case Var = bar() of _ -> Var end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of _ -> receive Var -> Var end end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of Var -> Var end.\nbar() -> 1.\n";

  private void doTestFromEachOccurrence(String text) {
    List<Integer> offsets = varOffsets(text);
    String expected = text.replaceAll("\\bVar\\b", "NewVar");
    for (int offset : offsets) {
      myFixture.configureByText("a.erl", text.substring(0, offset) + "<caret>" + text.substring(offset));
      myFixture.renameElementAtCaret("NewVar");
      assertEquals("Rename from offset " + offset, expected, myFixture.getEditor().getDocument().getText());
    }
  }

  private static List<Integer> varOffsets(String text) {
    List<Integer> offsets = new ArrayList<>();
    Matcher matcher = Pattern.compile("\\bVar\\b").matcher(text);
    while (matcher.find()) offsets.add(matcher.start());
    return offsets;
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
