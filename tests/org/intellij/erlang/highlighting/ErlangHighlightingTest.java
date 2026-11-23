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

package org.intellij.erlang.highlighting;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.inspection.ErlangUnusedTypeInspection;
import org.intellij.erlang.psi.ErlangExportType;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.psi.ErlangTypeRef;

import java.util.ArrayList;
import java.util.List;

public class ErlangHighlightingTest extends ErlangHighlightingTestBase {
  public void testHelloWorld()        { doTest(); }
  public void testExport()            { doTest(); }
  public void testIsDigits()          { doTest(); }
  public void testTest()              { doTest(); }
  public void testRecords()           { doTest(); }
  public void testMp4Mux()            { doTest(); }
  public void testRecord()            { doTest(); }
  public void testQuoteUnquote()      { doTest(); }
  public void test23()                { doTest(); }
  public void testRecordsResolve()    { doTest(); }
  public void testNativeRecordsResolve() { doTest("native_records_resolve.erl", "native_records_remote.erl"); }
  public void testExportResolve()     { doTest(); }
  public void testUnusedFunction()    { doTest(); }
  public void testUnusedFunctionEunitIsNotTheFirstIncludeLib() { doTest("eunit_not_first_include_lib_tests.erl"); } // issue 1434
  public void testDuplicate()         { doTest(); }
  public void testmod2()              { doTest(); } // issue 29
  public void testx()                 { doTest(); } // issue 38
  public void testUnresolvedField()   { doTest(); }
  public void test34()                { doTest(); }
  public void test124()               { doTest(); }
  public void test149()               { doTest(); }
  public void testMismatchedHead()    { doTest(); }
  public void test154()               { doTest(); }
  public void test154_2()             { doTest(); }
  public void test155()               { doTest(); }
  public void test158()               { doTest(); }
  public void testIoFormat()          { doTest(); }
  public void testSuppression()       { doTest(); }
  public void testVariableResolve()   { doTest(); }
  public void testBinaryComprehensionTemplate() { doTest(); }
  public void test190()               { doTest(); }
  public void testMacrosResolve()     { doTest(); }
  public void testIfConditionalCompilation() { doTest(); }
  public void test200()               { doTest(); }
  public void testSpawnTest()         { doTest(); }
  public void test211()               { doTest(); }
  public void test217()               { doTest(); }
  public void testLager()             { doTest(); }
  public void test202()               { doTest(); }
  public void test221()               { doTest(); }
  public void testInclude()           { doTest(); }
  public void testIncludeLib()        { doTest(); }
  public void testIllegalGuard()      { doTest(); }
  public void testIllegalPattern()    { doTest(); }
  public void testIllegalPatternMacroBinary() { doTest(); } // issue 1125
  public void testIllegalPatternMacroListOp() { doTest(); } // issue 890
  public void testIllegalPatternListOpMinus() { doTest(); }
  public void testIllegalPatternListOpRight() { doTest(); }
  public void testIllegalPatternQualifiedMacroCall() { doTest(); }
  public void testIllegalPatternListPrefix() { doTest(); }
  public void testIllegalPatternCatchClass() { doTest(); }
  public void testIllegalPatternMapKey() { doTest(); }
  public void testIllegalPatternPredefinedMacro() { doTest(); }
  public void testIllegalPatternBinarySegmentListOp() { doTest(); }
  public void testIllegalPatternCatchClassMatch() { doTest(); }
  public void testIllegalPatternQuotedPredefinedMacro() { doTest(); }
  public void testIllegalPatternNestedMapKey() { doTest(); }
  public void testIllegalPatternMapKeyRecordGuard() { doTest(); }
  public void testIllegalPatternGuardMatch() { doTest(); }
  public void testIllegalPatternCatchClassMacroCall() { doTest(); }
  public void testUnresolvedFunction(){ doTest(); }
  public void test354()               { doTest(); }
  public void test364()               { doTest(); }
  public void test365()               { doTest(); }
  public void test387()               { doTest(); }
  public void testMultiTarget()       { doTest(); }
  public void testInFunClause()       { doTest(); }
  public void testMapKeyInFunHead()   { doTest(); }
  public void testNamedFunVariables() { doTest(); }
  public void testAnonymousFunLaterClauseVariables() { doTest(); }
  public void testDuplicateExport1()  { doTest(); }
  public void testDuplicateExport2()  { doTest(); }
  public void testArityDigitSeparators() { doTest(); }
  public void testDefineImported1()   { doTest(); }
  public void testDefineImported2()   { doTest(); }

  public void testAutoimportCall1()   { doTest(); }
  public void testAutoimportCall2()   { doTest(); }
  public void testAutoimportCall3()   { doTest(); }
  public void testNotAutoImportedCall() { doTest(); }
  public void testImportedNotAutoImportedBif() { doTest("ImportedNotAutoImportedBif.erl", "ImportedNotAutoImportedBif.hrl"); }

  public void testNoAutoImport1()     { doTest(); }
  public void testNoAutoImport2()     { doTest(); }
  public void testNoAutoImport3()     { doTest(); }
  public void testNoAutoImport4()     { doTest(); }
  public void testNoAutoImport5()     { doTest(); }
  public void testNoAutoImport6()     { doTest(); }
  public void testNoAutoImport7()     { doTest(); }
  public void testNoAutoImport8()     { doTest(); }
  public void test605()               { doTest(); }
  public void test821()               { doTest(); }
  public void test933()               { doTest(); }
  public void testFunVarArityInMacroArguments() { doTest(); }
  public void testFunVarArityInConstantMacroArguments() { doTest(); }
  public void testFunVarArityInMacroArgumentsInTypes() { doTest(); }
  public void testFunVarArityInPredefinedMacroArguments() { doTest(); }
  public void test536()               { doTest("536.erl", "536.hrl"); }
  public void testExportPseudoFunctions() { doTest(); }
  public void testIncorrectModuleName() { doTest(); }
  public void testModuleAttributeInHeader() { doTest("ModuleAttributeInHeader.hrl"); }
  public void testCaseHeadVariable()  { doTest(); } // issue 982
  public void testOptionalCallbacksTuple() { doTest(); } // issue 1120

  public void testAttributeAnnotations() {
    String testDataFile = getTestName(false) + ".erl";
    myFixture.configureByFile(testDataFile);
    myFixture.checkHighlighting(false, true, false, false);
  }

  public void testIfAttributeAnnotations() {
    testAttributeAnnotations();
  }

  private void doTestWithInclude() {
    myFixture.configureByText("incl.erl",
                              """
                                -module(incl).
                                -export([crc32/1, abs/1, dt_get_tag/0, bar/0, abs/0]).

                                crc32(Data) -> Data.
                                abs(D) -> D.
                                abs() -> zero.
                                dt_get_tag() -> ok.
                                bar() -> ok.""");
    doTest();
  }

  public void testImportAutoimported()    { doTestWithInclude(); }

  public void test940() {
    myFixture.configureByText("ets.erl", "-module(ets).\n-export([fun2ms/1]).\nfun2ms(F) -> F.");
    myFixture.addFileToProject("stdlib/include/ms_transform.hrl", "-compile({parse_transform, ms_transform}).");
    doTest();
  }

  public void test940WithoutMsTransform() {
    myFixture.configureByText("ets.erl", "-module(ets).\n-export([fun2ms/1]).\nfun2ms(F) -> F.");
    doTest();
  }

  public void test940Review() {
    myFixture.configureByText("ets.erl", "-module(ets).\n-export([fun2ms/1]).\nfun2ms(F) -> F.");
    doTest();
  }

  public void test940Dbg() {
    myFixture.configureByText("ets.erl", "-module(ets).\n-export([fun2ms/1]).\nfun2ms(F) -> F.");
    myFixture.configureByText("dbg.erl", "-module(dbg).\n-export([fun2ms/1]).\nfun2ms(F) -> F.");
    doTest();
  }

  public void testErlang17SyntaxError() {
    enableErlang17SyntaxInspection();
    doTest();
  }

  public void testErlang18SyntaxError() {
    enableErlang18SyntaxInspection();
    doTest();
  }

  public void testErlang27SyntaxError() {
    enableErlang27SyntaxInspection();
    doTest();
  }

  public void testErlang28SyntaxError() {
    enableErlang28SyntaxInspection();
    doTest();
  }

  public void testErlang29SyntaxError() {
    enableErlang29SyntaxInspection();
    doTest();
  }

  public void testUnresolvedMacros()  {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testBinaryTypeSizeMacros() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosInIfdef() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosInConditionalBranches() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosInIfInsideIfdef() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosUndefInInclude() {
    enableUnresolvedMacroInspection();
    myFixture.configureByText("undef_m.hrl", "-undef(M).");
    doTest();
  }

  public void testUnresolvedMacrosQuotedInIfdef() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosOtpRelease() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testMacroThenStringLiteral() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testAlreadyImported1()      { doTestWithInclude(); }
  public void testAlreadyImported2()      { doTestWithInclude(); }
  public void test176() {                    
    myFixture.configureByText("aaa.hrl", "foo() -> ok.");
    doTest();
  }

  public void testNoHighlightingInsideMacroCalls() {
    enableUnresolvedMacroInspection();
    enableErlang17SyntaxInspection();
    doTest();
  }

  public void testIncludeResolve()             { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testIncludeLibResolve()          { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRecursiveIncludeResolve()    { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRecursiveIncludeLibResolve() { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRelativePathInclude()        { doTestWithApp(); }

  public void testFunctionImportFromTransitiveInclusion() { doTestWithApp(); }
  public void testFunctionImportFromInclusion()           { doTestWithApp(); }
  public void testExportFromInclusion()                   { doTestWithApp(); }

  // issue 1295: a chain of n left-associative operators is a PSI tree n levels deep
  public void testLongOperatorChain() {
    myFixture.configureByText("chain_12000.erl",
                              "-module(chain_12000).\n-export([f/0]).\nf() -> 1" + " + 1".repeat(12000) + ".\n");
    myFixture.checkHighlighting();
  }

  // issue 1435: types with the same name and different arities are distinct types
  public void testTypesWithSameNameAndDifferentArity() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    doTest();
  }

  // issue 1435
  public void testTypeReferenceResolvesByArity() {
    myFixture.configureByFile("TypesWithSameNameAndDifferentArity.erl");
    ErlangTypeRef useOfTreeA = null;
    for (ErlangTypeRef ref : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangTypeRef.class)) {
      if (ref.getParent().getText().equals("tree(A)")) useOfTreeA = ref;
    }
    assertNotNull(useOfTreeA);
    PsiElement resolved = useOfTreeA.getReference().resolve();
    assertInstanceOf(resolved, ErlangTypeDefinition.class);
    assertEquals(1, ((ErlangTypeDefinition) resolved).getArity());
  }

  public void testExportTypeResolvesByArity() {
    myFixture.configureByText("ov.erl", "-module(ov).\n-export_type([tree/1, tree/0]).\n" +
                                        "-type tree() :: tree(term()).\n-type tree(A) :: {node, A, [tree(A)]} | nil.\n");
    List<ErlangExportType> exports = new ArrayList<>(PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangExportType.class));
    assertEquals(2, exports.size());
    assertResolvesToArity(exports.get(0).getReference(), 1);
    assertResolvesToArity(exports.get(1).getReference(), 0);
  }

  public void testRemoteTypeResolvesByArity() {
    myFixture.addFileToProject("ov.erl", "-module(ov).\n-export_type([tree/0, tree/1]).\n" +
                                         "-type tree() :: tree(term()).\n-type tree(A) :: {node, A, [tree(A)]} | nil.\n");
    myFixture.configureByText("user.erl", "-module(user).\n-type t() :: ov:tree(atom()).\n-type u() :: ov:tree().\n");
    assertResolvesToArity(findTypeRef("ov:tree(atom())").getReference(), 1);
    assertResolvesToArity(findTypeRef("ov:tree()").getReference(), 0);
  }

  public void testIncludedTypeResolvesByArity() {
    myFixture.addFileToProject("ov.hrl", "-type tree() :: tree(term()).\n-type tree(A) :: {node, A, [tree(A)]} | nil.\n");
    myFixture.configureByText("user.erl", "-module(user).\n-include(\"ov.hrl\").\n-type t() :: tree(atom()).\n-type u() :: tree().\n");
    assertResolvesToArity(findTypeRef("tree(atom())").getReference(), 1);
    assertResolvesToArity(findTypeRef("tree()").getReference(), 0);
  }

  // issue 1505: erlc only warns for BIFs auto-imported since R14 (erl_internal:old_bif/2 is false)
  public void testOverriddenBifAutoImportedSinceR14() {
    List<HighlightInfo> infos = doHighlightingOf("OverriddenBifAutoImportedSinceR14.erl");
    for (HighlightInfo info : infos) {
      assertFalse(info.getDescription(), info.getSeverity() == HighlightSeverity.ERROR);
      assertFalse(info.getDescription(), String.valueOf(info.getDescription()).contains("pre R14"));
    }
  }

  // issue 1505
  public void testImportOverridesBifAutoImportedSinceR14() {
    for (HighlightInfo info : doHighlightingOf("ImportOverridesBifAutoImportedSinceR14.erl")) {
      assertFalse(info.getDescription(), String.valueOf(info.getDescription()).contains("pre R14"));
    }
  }

  // issue 1505: control, erlc gives an error for a pre-R14 auto-imported BIF
  public void testOverriddenPreR14AutoImportedBif() {
    boolean found = false;
    for (HighlightInfo info : doHighlightingOf("OverriddenPreR14AutoImportedBif.erl")) {
      if (info.getSeverity() == HighlightSeverity.ERROR &&
          "Ambiguous call of overridden pre R14 auto-imported BIF 'length/1'".equals(info.getDescription())) {
        found = true;
      }
    }
    assertTrue(found);
  }

  private List<HighlightInfo> doHighlightingOf(String file) {
    myFixture.configureByFile(file);
    setUpInspections(myFixture);
    return myFixture.doHighlighting();
  }

  private ErlangTypeRef findTypeRef(String typeText) {
    for (ErlangTypeRef ref : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangTypeRef.class)) {
      if (ref.getParent().getText().equals(typeText)) return ref;
    }
    fail("no type " + typeText);
    return null;
  }

  private static void assertResolvesToArity(PsiReference reference, int arity) {
    PsiElement resolved = reference.resolve();
    assertInstanceOf(resolved, ErlangTypeDefinition.class);
    assertEquals(arity, ((ErlangTypeDefinition) resolved).getArity());
  }
}
