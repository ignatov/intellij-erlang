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

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.inspection.ErlangIllegalGuardInspection;
import org.intellij.erlang.inspection.ErlangUnusedTypeInspection;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
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
  public void testUnusedFunctionEunitHrlIncludedWithInclude() { doTest("include_eunit_hrl_tests.erl", "eunit.hrl"); } // issue 1504
  public void testDuplicate()         { doTest(); }
  public void testDuplicateAfterSplitForm() { doTest(); } // issue 1609
  public void testLoneDot()           { doTest(); } // issue 1646
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
  public void testIoFormatTwoArgumentControls() { doTest(); }
  public void testIoFormatTwoArgumentControlsOneArgument() { doTest(); }
  public void testSuppression()       { doTest(); }
  public void testVariableResolve()   { doTest(); }
  public void testBinaryComprehensionTemplate() { doTest(); }
  public void test190()               { doTest(); }
  public void testMacrosResolve()     { doTest(); }
  public void testIfConditionalCompilation() { doTest(); }
  public void testIfElifDefinedMacro() { doTest(); } // issue 1636
  public void testGeneratorExpressionDoesNotSeeOwnPattern() { doTest(); } // issue 1641
  public void testGeneratorPatternScope() { doTest(); } // issue 1641
  public void testRepeatedGeneratorPatterns() { doTest(); } // issue 1641
  public void testGeneratorMapKeyPattern() { doTest(); } // issue 1641
  public void testDefinedOutsideIfElif() { doTest(); } // issue 1636
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
  // issue 1506: ceil/1 and floor/1 are guard BIFs since OTP 20, min/2 and max/2 since OTP 26
  public void testIllegalGuardCeilFloorMinMaxR15() {
    myFixture.configureByFile("IllegalGuardCeilFloorMinMaxR15.erl");
    myFixture.enableInspections(ErlangIllegalGuardInspection.class);
    myFixture.checkHighlighting(true, false, false);
  }

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
  public void testIllegalPatternComputedSizeAndKey() { doTest(); } // issue 1501
  public void testIllegalPatternComputedSizeAndKeyValue() { doTest(); }
  public void testUnresolvedFunction(){ doTest(); }
  public void testUnresolvedBnotOperator() { doTest(); } // issue 1600
  public void testIllegalGuardBnotOperator() { doTest(); } // issue 1600
  public void test354()               { doTest(); }
  public void test364()               { doTest(); }
  public void test365()               { doTest(); }
  public void test387()               { doTest(); }
  public void testMultiTarget()       { doTest(); }
  public void testInFunClause()       { doTest(); }
  public void testMapKeyInFunHead()   { doTest(); }
  public void testNamedFunVariables() { doTest(); }
  public void testNamedFunUnusedName() { doTest(); } // issue 1642
  public void testAnonymousFunLaterClauseVariables() { doTest(); }
  public void testFunHeadRepeatedVariable() { doTest(); } // issue 1640
  public void testFunArgumentShadowsEarlierGeneratorVariable() { doTest(); } // issue 1652
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
  public void testBasedIntegers()         { doTest(); }
  public void testBasedIntegersErrors()   { doTest(); }
  public void testHexEscapeCodePoints()   { doTest(); }
  public void testHexEscapeCodePointsErrors() { doTest(); }
  public void testIncorrectModuleName() { doTest(); }
  public void testModuleAttributeInHeader() { doTest("ModuleAttributeInHeader.hrl"); }
  public void testCaseHeadVariable()  { doTest(); } // issue 982
  public void testUnboundInRecursiveFun() { doTest(); } // issue 1650
  public void testUnboundInOwnMatchRight() { doTest(); } // issue 1650
  public void testUnboundInGuardBoundInBody() { doTest(); } // issue 1650
  public void testUnboundInOwnBindingNeighbours() { doTest(); } // issue 1650
  public void testUnboundInOwnBindingClausePattern() { doTest(); } // issue 1650
  public void testUnboundInOwnBindingCasePatternTwice() { doTest(); } // issue 1650
  public void testUnboundInOwnBindingReceive() { doTest(); } // issue 1650
  public void testCaseClauseRematchTwice() { doTest(); } // issue 1651
  public void testCaseClauseUseBeforeMatch() { doTest(); } // issue 1651
  public void testCaseClauseNestedCasePattern() { doTest(); } // issue 1651
  public void testCatchClassVarAssignment() { doTest(); } // issue 1648
  public void testOptionalCallbacksTuple() { doTest(); } // issue 1120
  public void testFunArgShadowsOtherCaseClauseVariable() { doTest(); } // issue 1654

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

  // issue 1596
  public void testUnresolvedMacrosQuotedPredefined() {
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

  // issue 1502: erlc never warns about unused types of an included file
  public void testNoUnusedTypeInHeader() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    doTest("UnusedTypeInHeader.hrl", "unused_type_in_header_user.erl");
  }

  // issue 1561
  public void testModuleMacroQualifiedType() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    myFixture.configureByText("own.erl", """
      -module(own).
      -export([f/0]).
      -type priv() :: integer().
      -type <warning descr="Unused type 'unused'">unused</warning>() :: atom().
      -spec f() -> ?MODULE:priv().
      f() -> 1.""");
    myFixture.checkHighlighting();
    ErlangTypeRef ref = null;
    for (ErlangTypeRef r : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangTypeRef.class)) {
      if (r.getParent().getText().equals("?MODULE:priv()")) ref = r;
    }
    assertNotNull(ref);
    PsiElement resolved = ref.getReference().resolve();
    assertInstanceOf(resolved, ErlangTypeDefinition.class);
    assertEquals("priv", ((ErlangTypeDefinition) resolved).getName());
  }

  // issue 1561: ?MOD:t() is not a use of the local type t
  public void testOtherMacroQualifiedTypeIsNotUseOfLocalType() {
    doTestOtherMacroQualifiedTypeIsNotUseOfLocalType();
  }

  public void testOtherMacroQualifiedTypeIsNotUseOfLocalTypeWithoutMacroLayer() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    doTestOtherMacroQualifiedTypeIsNotUseOfLocalType();
  }

  private void doTestOtherMacroQualifiedTypeIsNotUseOfLocalType() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    myFixture.addFileToProject("other.erl", "-module(other).\n-export_type([t/0]).\n-type t() :: atom().\n");
    myFixture.configureByText("own.erl", """
      -module(own).
      -export_type([u/0]).
      -define(MOD, other).
      -type <warning descr="Unused type 't'">t</warning>() :: integer().
      -type u() :: ?MOD:t().""");
    myFixture.checkHighlighting();
  }

  // issue 1554: a bare atom in a type is not a use of the type with the same name
  public void testAtomInTypeIsNotUseOfType() {
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

  // issue 1553: erlc reports "type t() undefined" and "type t(_,_,_) undefined" here
  public void testTypeWithUndefinedArityDoesNotResolve() {
    myFixture.configureByText("ar.erl", "-module(ar).\n-export_type([t/3]).\n-type t(A) :: {A}.\n-type u() :: t().\n");
    ErlangExportType export = PsiTreeUtil.findChildOfType(myFixture.getFile(), ErlangExportType.class);
    assertNotNull(export);
    PsiElement exported = export.getReference().resolve();
    PsiElement used = findTypeRef("t()").getReference().resolve();
    assertTrue("t/3 resolved to " + describe(exported) + ", t() resolved to " + describe(used), exported == null && used == null);
  }

  public void testTypeWithUndefinedArityIsUnused() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    myFixture.configureByText("ar.erl", "-module(ar).\n-export_type([t/3, u/0]).\n" +
                                        "-type <warning descr=\"Unused type 't'\">t</warning>(A) :: {A}.\n-type u() :: t().\n");
    myFixture.checkHighlighting(true, false, false);
  }

  public void testRemoteTypeWithUndefinedArityDoesNotResolve() {
    myFixture.addFileToProject("ov.erl", "-module(ov).\n-export_type([tree/1]).\n-type tree(A) :: {node, A}.\n");
    myFixture.configureByText("user.erl", "-module(user).\n-type u() :: ov:tree().\n");
    assertNull(findTypeRef("ov:tree()").getReference().resolve());
  }

  public void testIncludedTypeWithUndefinedArityDoesNotResolve() {
    myFixture.addFileToProject("ov.hrl", "-type tree(A) :: {node, A}.\n");
    myFixture.configureByText("user.erl", "-module(user).\n-include(\"ov.hrl\").\n-type u() :: tree().\n");
    assertNull(findTypeRef("tree()").getReference().resolve());
  }

  // a macro argument can expand to several arguments: ?TWO makes t(?TWO) a use of t/2
  public void testRenameTypeUsedWithMacroArgumentInHeader() {
    myFixture.configureByText("h.hrl", "-define(TWO, a, b).\n-type <caret>t(A, B) :: {A, B}.\n-type u() :: t(?TWO) | t(a, b).\n");
    myFixture.renameElementAtCaret("zz");
    myFixture.checkResult("-define(TWO, a, b).\n-type zz(A, B) :: {A, B}.\n-type u() :: zz(?TWO) | zz(a, b).\n");
  }

  public void testTypeUsedWithMacroArgumentIsNotUnused() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    myFixture.configureByText("ar.erl", "-module(ar).\n-export_type([u/0]).\n-define(TWO, a, b).\n" +
                                        "-type t(A, B) :: {A, B}.\n-type u() :: t(?TWO).\n");
    myFixture.checkHighlighting(true, false, false);
  }

  // a macro argument still prefers the counted arity: t(?X) is a use of t/1 even when t/0 comes first
  public void testTypeUsedWithMacroArgumentPrefersCountedArity() {
    myFixture.configureByText("c.erl", "-module(c).\n-export_type([u/0]).\n-define(X, a).\n" +
                                       "-type t() :: a.\n-type t(A) :: {A}.\n-type u() :: t(?X).\n");
    assertResolvesToArity(findTypeRef("t(?X)").getReference(), 1);
  }

  public void testTypeUsedWithMacroArgumentLeavesOtherArityUnused() {
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
    myFixture.configureByText("c.erl", "-module(c).\n-export_type([u/0]).\n-define(X, a).\n" +
                                       "-type <warning descr=\"Unused type 't'\">t</warning>() :: a.\n-type t(A) :: {A}.\n-type u() :: t(?X).\n");
    myFixture.checkHighlighting(true, false, false);
  }

  public void testRenameOtherArityKeepsTypeUsedWithMacroArgumentInHeader() {
    myFixture.configureByText("h.hrl", "-define(X, a).\n-type <caret>t() :: a.\n-type t(A) :: {A}.\n-type u() :: t(?X).\n");
    myFixture.renameElementAtCaret("zz");
    myFixture.checkResult("-define(X, a).\n-type zz() :: a.\n-type t(A) :: {A}.\n-type u() :: t(?X).\n");
  }

  private static String describe(PsiElement element) {
    return element instanceof ErlangTypeDefinition type ? type.getName() + "/" + type.getArity() : String.valueOf(element);
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
