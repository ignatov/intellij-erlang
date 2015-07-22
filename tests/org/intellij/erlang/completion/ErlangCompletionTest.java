/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.openapi.util.Condition;
import com.intellij.util.ArrayUtilRt;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.List;

public class ErlangCompletionTest extends ErlangCompletionTestBase {
  public void testKeywords1() { doTestInclude("-<caret>", "module", "record", "define"); }
  public void testVariablesFromDefinition() { doTestInclude("foo(A, B, C)-> <caret>", "A", "B", "C"); }
  public void testVariablesFromBody() { doTestInclude("foo(A, B, C)-> D=1, <caret>", "A", "B", "C", "D"); }
  public void testFunctions() {
    doTestInclude(
      "foo() -> ok.\n" +
      "buzz() -> ok.\n" +
      "bar(A)-> <caret>", "foo", "buzz");
  }

  public void testFunctionCompletionInTypedList() {
    doTestInclude("foo() -> ok. -record(state, {first = <caret>}).",
      "begin", "try", "fun", "if",
      "node", "pid_to_list", "spawn", "binary_to_list",
      "foo");
  }

  public void testRecords() {
    doTestInclude(
      "-record(foo, {id}).\n" +
      "-record(buz, {id}).\n" +
      "bar(A)-> A#<caret>", "foo", "buz");
  }  
  
  public void testRecordFields() {
    doTestEquals(
      "-record(foo, {id, two}).\n" +
      "bar(A)-> #foo{<caret>}", "id", "two");
  }
  
  public void testRecordFields2() {
    doTestEquals(
      "-record(foo, {id, two}).\n" +
      "bar(A)-> #foo{two=1,<caret>}", "id", "two");
  }
  
  public void testRecordFields3() {
    doTestInclude(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> #foo{two= <caret>}", "A", "B");
  }
  
  public void testRecordFields4() {
    doTestInclude(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> #foo{two=<caret>}", "A", "B");
  }  
  
  public void testRecordFields5() {
    doCheckResult(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> A#foo.tw<caret>", 
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo.two");
  }

  public void testRecordFields6() {
    doCheckResult(
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo{tw<caret>}",
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo{two = }");
  }

  public void testMacros() {
    doTestInclude(
      "-define(foo, 1).\n" +
      "-define(buz, 1).\n" +
      "bar(A)-> ?<caret>", "foo", "buz");
  }

  public void testTypesInRecords() {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-record(rec, {id :: <caret>}).", "foo", "buz");
  }

  public void testBuiltInTypesInRecords() {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-record(rec, {id :: <caret>}).",
      ArrayUtilRt.toStringArray(ErlangPsiImplUtil.BUILT_IN_TYPES)
    );
  }

  public void testTypesInSpec() {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-spec my_fun(<caret>)", "foo", "buz", "atom", "no_return");
  }

  public void testTypesInTypeDeclaration() {
    doTestInclude(
      "-type foo() :: <caret>atom().\n" +
        "-type buz() :: string().\n" +
        "-type tes() :: <caret>)", "foo", "buz", "atom", "no_return");
  }

  public void testBif() {
    doTestInclude("foo() -> <caret>", "is_function", "is_record", "universaltime_to_posixtime");
  }

  public void testBifFromModules() {
    doTestInclude("foo() -> lists:<caret>", "member", "reverse", "keysearch");
  }

  public void testMultiModule() {
    myFixture.configureByFiles("multi-module/a.erl");
    myFixture.configureByFile("multi-module/b.erl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo",
      "module_info", "module_info");
    // means "bar/1", "bar/0", "foo/1", "foo/0", "module_info/0", "module_info/1"
  }

  public void testBifImport() {
    doTestInclude("-import(math, [<caret>]).", "sin", "sqrt");
  }

  public void testBifImport2() {
    doTestInclude("-import(math, [sin/1, sqrt/1]).\n" +
      "foo() -> <caret>", "sin", "sqrt");
  }

  public void test182() {
    doTestInclude("test() -> <caret>\n" +
      "ok.\n" +
      "my_local_function() -> not_so_ok.",
      "my_local_function");
  }

  public void testNoVariableDuplicates() {
    myFixture.configureByText("a.erl", 
      "foo() ->\n" +
      "    case {1, 1} of\n" +
      "        {A, A} -> <caret>\n" +
      "    end.");
    myFixture.complete(CompletionType.BASIC, 1);
    List<String> stringList = myFixture.getLookupElementStrings();
    assertNotNull(stringList);
    List<String> vars = ContainerUtil.filter(stringList, new Condition<String>() {
      @Override
      public boolean value(String s) {
        return s.equals("A");
      }
    });
    assertSize(1, vars);
  }

  public void testIncludeLib()  { doCheckResult("-include_<caret>", "-include_lib(\"<caret>\")."); }
  public void testInclude()     { doCheckResult("-inclu<caret>", "-include(\"<caret>\").", '('); }
  public void testExport()      { doCheckResult("-exp<caret>", "-export([<caret>]).", '('); }
  public void testExportType()  { doCheckResult("-export_t<caret>", "-export_type([<caret>])."); }
  public void testBehaviour()   { doCheckResult("-behaviou<caret>", "-behaviour(<caret>)."); }
  public void testBehavior()    { doCheckResult("-behavior<caret>", "-behavior(<caret>)."); }

  public void testExportFunction() {
    doCheckResult("-export([<caret>]). foo(A, B, C) -> ok.", "-export([foo/3<caret>]). foo(A, B, C) -> ok.", Lookup.COMPLETE_STATEMENT_SELECT_CHAR);
  }

  public void testLager() {
    doTestInclude("foo() -> lager:<caret>", "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency");
  }

  public void testImportModule() {
    myFixture.configureByFiles("multi-module/a.erl");
    myFixture.configureByFile("multi-module/b.erl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo",
      "module_info", "module_info");
    // means "bar/1", "bar/0", "foo/1", "foo/0", "module_info/0", "module_info/1"
  }

  public void testModuleCompletionContainsModule() {
    myFixture.configureByFiles("module-completion/use_module.erl", "module-completion/test_module.erl");
    doTestVariantsInner(CompletionType.BASIC, 2, CheckType.INCLUDES, "test_module");
  }

  public void testModuleCompletionContainsFunctions() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestInclude("foo() -> fake_mod<caret>", "fake_module:bar");
  }

  public void testModuleCompletionExcludeFunctions() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestVariants("foo() -> fake_mod<caret>", CompletionType.BASIC, 1, CheckType.EXCLUDES, "far", "fake_module:far");
  }

  public void testFunctionCompletionByPartialName() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestInclude("foo() -> fmba<caret>", "fake_module:bar");
  }

  public void testFunctionCompletionExcludeByPartialName() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestVariants("foo() -> fmba<caret>", CompletionType.BASIC, 1, CheckType.EXCLUDES,
      "tar", "fake_module:tar",
      "far", "fake_module:far");
  }

  public void testFunctionCompletionByPartialNameWithColon() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestInclude("foo() -> fm:ba<caret>", "fake_module:bar");
  }

  public void testFunctionCompletionCheckFirst() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    myFixture.configureByText("a.erl", "bar() -> ok. foo() -> bar<caret>");
    myFixture.complete(CompletionType.BASIC, 1);
    List<String> compList = myFixture.getLookupElementStrings();
    assertNotNull(compList);
    assertEquals(compList.get(0), "bar");
  }

  public void testFunctionExpandByPartialName() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doCheckResult("foo() -> fmta<caret>", "foo() -> fake_module:tar()<caret>");
  }

  public void testModuleNameIsMatchedFromTextBeforeColon() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestEquals("foo() -> famo:<caret>",
      "fake_module:bar", "fake_module:bar", "fake_module:tar", "module_info", "module_info");
    // means "fake_module:bar/0", "fake_module:bar/1", "fake_module:tar/0", "module_info/0", "module_info/1"
  }

  public void testModuleNameIsMatchedFromTextBeforeColonAtComma() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestEquals("foo() -> famo:<caret>, ok.",
      "fake_module:bar", "fake_module:bar", "fake_module:tar", "module_info", "module_info");
    // means "fake_module:bar/0", "fake_module:bar/1", "fake_module:tar/0", "module_info/0", "module_info/1"
  }

  public void testModuleNameIsMatchedFromTextBeforeColonAtIncompleteClause() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestEquals("foo() -> famo:<caret> ok.",
      "fake_module:bar", "fake_module:bar", "fake_module:tar", "module_info", "module_info");
    // means "fake_module:bar/0", "fake_module:bar/1", "fake_module:tar/0", "module_info/0", "module_info/1"
  }

  public void testModuleNameIsMatchedFromTextBeforeColonWithPartialMatch() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestEquals("foo() -> famo:ba<caret>ckend", "fake_module:bar", "fake_module:bar");
    // means "fake_module:bar/0", "fake_module:bar/1"
  }

  public void testModuleFunctionCompletionForEmptyText() {
    myFixture.configureByFiles("module-completion/fake_module.erl");
    doTestInclude("foo() -> <caret>.", "fake_module", "fake_module:bar", "fake_module:tar", "finish_after_on_load");
  }
  
  public void testModuleFunctionCompletionQuoted() {
    myFixture.configureByText("OTP-PUB-KEY.erl", "-module('OTP-PUB-KEY'). -export(['dec_D-1'/2]). 'dec_D-1'(Tlv, TagIn) -> 1.");
    doCheckResult("foo() -> Odec<caret>", "foo() -> 'OTP-PUB-KEY':'dec_D-1'(<caret>)");
  }

  public void testModuleFunctionCompletionQuoted2() {
    myFixture.configureByText("OTP-PUB-KEY.erl", "-module('OTP-PUB-KEY'). -export([dec_D1/2]). dec_D1(Tlv, TagIn) -> 1.");
    doCheckResult("foo() -> Odec<caret>", "foo() -> 'OTP-PUB-KEY':dec_D1(<caret>)");
  }
  
  public void testModuleFunctionCompletionQuoted3() {
    myFixture.configureByText("OTP-PUB-KEY.erl", "-module('OTP-PUB-KEY'). -export(['dec_D1'/2]). 'dec_D1'(Tlv, TagIn) -> 1.");
    doCheckResult("foo() -> Odec<caret>", "foo() -> 'OTP-PUB-KEY':dec_D1(<caret>)");
  }
  
  public void testModuleFunctionCompletionQuoted4() {
    myFixture.configureByText("otp_pub_key.erl", "-module(otp_pub_key). -export(['dec_D-1'/2]). 'dec_D-1'(Tlv, TagIn) -> 1.");
    doCheckResult("foo() -> otpdec<caret>", "foo() -> otp_pub_key:'dec_D-1'(<caret>)");
  }
  
  public void testModuleCompletionWithColon() {
    myFixture.configureByFiles("module-completion/test_module.erl");
    doCheckResult("foo() -> test_modul<caret>", "foo() -> test_module:");
  }

  public void testModuleCompletionWithoutColon() {
    myFixture.configureByFiles("module-completion/test_module.erl");
    doCheckResult("foo() -> bar(test_modul<caret>", "foo() -> bar(test_module");
  }

  public void test176() {
    myFixture.configureByFiles("headers/a.erl", "headers/header.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "foo");
  }

  public void test465() {
    myFixture.configureByFiles("465/a.erl", "465/specs.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "type1");
  }

  public void testFunctionExpression() {
    doCheckResult("zoo() -> fun zo<caret>", "zoo() -> fun zoo/0");
  }

  public void testFunctionExpression2() {
    doCheckResult("foo() -> fun <caret>", "foo() -> fun foo/0", Lookup.NORMAL_SELECT_CHAR);
  }

  public void test211() {
    doTestInclude("-module(test, [Id, Name::string()]). foo() -> <caret>", "Id", "Name");
  }

  public void testNoCompletionInStrings() {
    doTestVariants("foo() -> \"<caret>\"", CompletionType.BASIC, 1, CheckType.EQUALS);
  }

  public void testNoCompletionInComments() {
    doTestVariants("% <caret>", CompletionType.BASIC, 1, CheckType.EQUALS);
  }

  public void testIncludeCompletion() throws Exception {
    localFileSystemSetUp();
    myFixture.configureByFiles("include/includeCompletion.erl", "include/include/header.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "include/");
  }

  public void testIncludeLibCompletion() {
    myFixture.configureByFiles("include-lib/includeLib.erl", "include-lib/testapp/ebin/testapp.app", "include-lib/testapp/include/includefile.hrl");
    myFixture.complete(CompletionType.BASIC);
    myFixture.checkResultByFile("include-lib/includeLib-after.erl");
  }

  public void testIncludeLibEmptyCompletion() {
    myFixture.configureByFiles("include-lib-empty/includeLib.erl",
                               "include-lib-empty/testapp/ebin/testapp.app",
                               "include-lib-empty/testapp/include/includefile.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "testapp/");
  }

  public void testSmartInteger() {
    doSmartTest("-spec g(A :: integer()) -> integer().\n" +
      "g(A) -> 1.\n" +
      "foo() ->\n" +
      "    B = 2 / 1,\n" +
      "    B2 = \"\",\n" +
      "    B4 = (1),\n" +
      "    B3 = 1 + 1*1,\n" +
      "    g(<caret>);",
      CheckType.EQUALS, "B4", "B3", "g");
  }

  public void testSmartCompositeTypes() {
    doSmartTest(
      "-spec new(Func::atom(), fun() | string()) -> integer().\n" +
        "new(Func, StubFun) ->\n" +
        "    Str = \"\",\n" +
        "    Fun = fun () -> ok end,\n" +
        "    Fun2 = fun () -> ok end,\n" +
        "    new(atom, <caret>);\n" +
        "new(Func, ClauseSpecs) -> ok.",
      CheckType.EQUALS, "Fun", "Fun2" , "Str"
    );
  }

  public void testCameCaseModules() {
    myFixture.configureByText("CamelCase.erl", "");
    myFixture.configureByText("a.erl", "bar() -> Cam<caret>");
    myFixture.complete(CompletionType.BASIC, 2);
    myFixture.checkResult("bar() -> 'CamelCase':<caret>");
  }

  public void testFunctionsFromCameCaseModule() {
    myFixture.configureByText("CamelCase.erl", "-module('CamelCase'). -export([foo/0]). foo() -> ok.");
    doTestInclude("bar() -> 'CamelCase':<caret>", "foo");
  }

  public void testOverrideInsideRecord() {
    myFixture.configureByText("a.erl", "bar(Record, Record2) -> Rec<caret>#data{}.");
    myFixture.completeBasic();
    myFixture.type('\t');
    myFixture.checkResult("bar(Record, Record2) -> Record#data{}.");
  }

  public void testFunctionTypeArguments() {
    doTestInclude("-spec foo(Type) -> ok when <caret>", "Type");
  }

  public void testMacroArguments() {
    doTestInclude("-define(M(Arg), <caret>", "Arg");
  }

  public void testFunctionImportsFromIncludes() {
    myFixture.configureByFiles("imports/test.erl", "imports/funs.erl", "imports/importFuns.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "fun_a");
  }

  public void testFunctionImportsFromTransitiveIncludes() {
    myFixture.configureByFiles("imports/testTransitive.erl", "imports/funs.erl",
      "imports/importFuns.hrl", "imports/transitiveImportFuns.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "fun_a");
  }

  public void testVariableDeclarationInFunctionArguments() {
    myFixture.configureByText("a.erl", "foo(FirstVar, First<caret>) -> ok.");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "FirstVar", "First");
  }

  public void testVariableDeclarationInLeftPartOfAssignment() {
    myFixture.configureByText("a.erl", "foo(FirstVar) -> First<caret> = FirstVar.");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "First", "FirstVar");
  }

  public void testVariableDeclarationInPatternInLeftPartOfAssignment() {
    myFixture.configureByText("a.erl", "foo(FirstVar) -> {ok, First<caret>} = {ok, FirstVar}.");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "First", "FirstVar");
  }

  public void testNoVariableDeclarationInRightPartOfAssignment() {
    myFixture.configureByText("a.erl", "foo(FirstVar, FirstVar1) -> {ok, FirstVar} = {ok, First<caret>}.");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EXCLUDES, "First");
  }
}