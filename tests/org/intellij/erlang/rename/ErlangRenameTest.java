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
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.inplace.MemberInplaceRenameHandler;
import com.intellij.testFramework.fixtures.CodeInsightTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangIncorrectModuleNameInspection;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangRenameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/rename/";
  }

  public void testUnquotedFunction()  { doTest("bar"); }
  public void testQuotedFunction()    { doTest("bar"); }
  public void testQuoteFunctionName() { doTest("BAR"); }
  public void testMultiClauseFunction() { doTest("BAR"); }

  public void testMultiClauseFunctionInplace() {
    myFixture.configureByText("a.erl", "-module(a).\n-export([foo/1]).\nfo<caret>o(0) -> ok;\n'foo'(N) -> foo(N - 1).\n");
    CodeInsightTestUtil.doInlineRename(new MemberInplaceRenameHandler(), "bar", myFixture);
    myFixture.checkResult("-module(a).\n-export([bar/1]).\nbar(0) -> ok;\nbar(N) -> bar(N - 1).\n");
  }

  public void testImportedFunctionInExport() {
    myFixture.configureByFiles("importedFunctionInExport.erl", "importedFunctionInExportUser.erl");
    myFixture.renameElementAtCaret("bar");
    myFixture.checkResultByFile("importedFunctionInExport-after.erl");
    myFixture.checkResultByFile("importedFunctionInExportUser.erl", "importedFunctionInExportUser-after.erl", false);
  }

  // issue 1561: ?MOD:t() names the type t of another module
  public void testTypeQualifiedWithOtherMacro() {
    myFixture.addFileToProject("other.erl", "-module(other).\n-export_type([t/0]).\n-type t() :: atom().\n");
    myFixture.testRename("typeQualifiedWithOtherMacro.erl", "typeQualifiedWithOtherMacro-after.erl", "zz");
  }

  public void testTypeQualifiedWithOtherMacroWithoutMacroLayer() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    testTypeQualifiedWithOtherMacro();
  }

  public void testUnquotedMacro()     { doTest("bar"); }
  public void testQuotedMacro()       { doTest("bar"); }
  public void testUnderscoreMacro()   { doTest("Foo"); }
  public void testMacrosInAttrs()     { doTest("BAR"); }
  
  public void testMultiTarget()       { doTest("Bar"); }
  public void testMultiTargetMacro()  { doTest("BAR"); }
  public void testTypedMacroRecordField() { doTest("uid"); }
  public void testTypedMacroRecordFieldFromUsage() { doTest("uid"); }
  public void testMacroNamedTypedRecordFieldFromUsage() { doTest("uid"); }
  public void testCaseHeadVariable()  { doTest("Renamed"); }
  public void testCaseHeadRematch()            { doTest("New"); }
  public void testCaseHeadRematchArgument()    { doTest("New"); }
  public void testCaseHeadRematchFromClause()  { doTest("New"); }
  public void testCaseHeadRematchInLaterCase() { doTest("New"); }
  public void testCaseHeadPattern()            { doTest("New"); }
  public void testCaseHeadPatternFromPattern() { doTest("New"); }
  public void testCaseClauseRematchTwice()     { doTest("New"); }
  public void testCaseClauseBodyRematchTwice() { doTest("New"); }
  public void testNamedFunShadowsFromCall()     { doTest("New"); }
  public void testNamedFunShadowsFromOuter()    { doTest("New"); }
  public void testNamedFunShadowsCaseHead()     { doTest("New"); }

  public void testMapKeyInFunHeadBody()           { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromKey()    { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromBody()   { doTest("Key2"); }
  public void testMapKeyInFunHeadGuard()          { doTest("Key2"); }
  public void testMapKeyInNestedFunHeads()        { doTest("Key2"); }
  public void testMapKeyInNestedFunHeadsFromOuter() { doTest("Key2"); }

  public void testNamedFunName()                  { doTest("Fact"); }
  public void testNamedFunShadowingArgument()     { doTest("Arg"); }
  public void testAnonymousFunLaterClauseVariable() { doTest("Zz"); }
  public void testGeneratorExpressionVariable()   { doTest("Zz"); } // issue 1641
  public void testGeneratorTemplateVariable()     { doTest("Zz"); } // issue 1641
  public void testLaterGeneratorPatternVariable() { doTest("Zz"); } // issue 1641
  public void testFilterVariableAfterTwoGenerators() { doTest("Zz"); } // issue 1641
  public void testGeneratorMapKeyFromEarlierGenerator() { doTest("Zz"); } // issue 1641
  public void testGeneratorMapKeyRenamedFromKey() { doTest("Zz"); } // issue 1641
  public void testFunArgumentShadowsGeneratorVariable() { doTest("Zz"); } // issue 1652
  public void testGeneratorVariableShadowedByFunArgument() { doTest("Zz"); } // issue 1652
  public void testFunHeadRepeatedVariable() { doTest("New"); } // issue 1640
  public void testFunHeadRepeatedVariableFromSecond() { doTest("B"); } // issue 1640
  public void testCatchClassVarAssignment()       { doTest("New"); } // issue 1648

  // issue 1295: a chain of n left-associative operators is a PSI tree n levels deep
  public void testLongOperatorChain() {
    String chain = " + 1".repeat(12000);
    myFixture.configureByText("chain_12000.erl", "-module(chain_12000).\n-export([f/0]).\n<caret>f() -> 1" + chain + ".\n");
    myFixture.renameElementAtCaret("g");
    myFixture.checkResult("-module(chain_12000).\n-export([g/0]).\ng() -> 1" + chain + ".\n");
  }
  
  // issue 1580: renaming a function replaces the name of each clause separately, so the time grows with the square of the clauses
  public void testManyClausesFunctionScalesLinearly() {
    renameManyClauses(500);
    long small = renameManyClauses(1500);
    long large = renameManyClauses(6000);
    assertTrue("renaming 4x as many clauses took " + small + " ms and " + large + " ms (" + large / Math.max(1, small) + "x)",
               large < 8 * Math.max(small, 100));
  }

  private long renameManyClauses(int n) {
    StringBuilder before = new StringBuilder("-module(scale_clauses).\n-export([f/1]).\n");
    StringBuilder after = new StringBuilder("-module(scale_clauses).\n-export([g/1]).\n");
    for (int i = 0; i < n; i++) {
      before.append(i == 0 ? "<caret>" : "").append("f(").append(i).append(") -> ").append(i).append(";\n");
      after.append("g(").append(i).append(") -> ").append(i).append(";\n");
    }
    myFixture.configureByText("scale_clauses.erl", before.append("f(_) -> ok.\n").toString());
    long start = System.nanoTime();
    myFixture.renameElementAtCaret("g");
    long ms = (System.nanoTime() - start) / 1_000_000;
    myFixture.checkResult(after.append("g(_) -> ok.\n").toString());
    return ms;
  }

  public void testRecordFromDefinition() {
    myFixture.configureByText("a.erl", "-record(<caret>foo, {a}).\nf() -> #foo{a = 1}.\n");
    myFixture.renameElementAtCaret("bar");
    myFixture.checkResult("-record(bar, {a}).\nf() -> #bar{a = 1}.\n");
  }

  public void testRecordFieldUsedInRecordType() {
    myFixture.configureByText("a.erl", "-record(r, {<caret>a, b}).\n-type t() :: #r{a :: integer()}.\nf() -> #r{a = 1}.\n");
    myFixture.renameElementAtCaret("c");
    myFixture.checkResult("-record(r, {c, b}).\n-type t() :: #r{c :: integer()}.\nf() -> #r{c = 1}.\n");
  }

  public void testTypeFromDefinition() {
    myFixture.configureByText("a.erl", "-type <caret>foo() :: ok.\n-spec f() -> foo().\nf() -> ok.\n");
    myFixture.renameElementAtCaret("bar");
    myFixture.checkResult("-type bar() :: ok.\n-spec f() -> bar().\nf() -> ok.\n");
  }

  // issue 1554: atoms with the name of the type are not uses of the type
  public void testTypeNotRenamedInAtomsOfTheSameName() {
    myFixture.configureByText("a.erl", "-type <caret>foo() :: [].\n-type t() :: foo | {foo} | foo().\n");
    myFixture.renameElementAtCaret("bar");
    myFixture.checkResult("-type bar() :: [].\n-type t() :: foo | {foo} | bar().\n");
  }

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

  public void testIncludedHeaderRename() {
    PsiFile header = myFixture.addFileToProject("header.hrl", "-define(HEADER, 1).\n");
    PsiFile includer = myFixture.addFileToProject("includer.erl", "-module(includer).\n-include(\"header.hrl\").\n");
    myFixture.renameElement(header, "renamed.hrl");
    assertEquals("-module(includer).\n-include(\"renamed.hrl\").\n", includer.getText());
  }

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
