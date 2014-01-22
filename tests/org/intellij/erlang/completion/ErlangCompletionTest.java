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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.openapi.util.Condition;
import com.intellij.util.ArrayUtilRt;
import com.intellij.util.PlatformUtilsCore;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.List;

public class ErlangCompletionTest extends ErlangCompletionTestBase {
  public ErlangCompletionTest() {
    super(PlatformUtilsCore.COMMUNITY_PREFIX);
  }

  public void testKeywords1() throws Throwable { doTestInclude("-<caret>", "module", "record", "define"); }
  public void testVariablesFromDefinition() throws Throwable { doTestInclude("foo(A, B, C)-> <caret>", "A", "B", "C"); }
  public void testVariablesFromBody() throws Throwable { doTestInclude("foo(A, B, C)-> D=1, <caret>", "A", "B", "C", "D"); }
  public void testFunctions() throws Throwable {
    doTestInclude(
      "foo() -> ok.\n" +
      "buzz() -> ok.\n" +
      "bar(A)-> <caret>", "foo", "buzz");
  }

  public void testRecords() throws Throwable {
    doTestInclude(
      "-record(foo, {id}).\n" +
      "-record(buz, {id}).\n" +
      "bar(A)-> A#<caret>", "foo", "buz");
  }  
  
  public void testRecordFields() throws Throwable {
    doTestEquals(
      "-record(foo, {id, two}).\n" +
      "bar(A)-> #foo{<caret>}", "id", "two");
  }
  
  public void testRecordFields2() throws Throwable {
    doTestEquals(
      "-record(foo, {id, two}).\n" +
      "bar(A)-> #foo{two=1,<caret>}", "id", "two");
  }
  
  public void testRecordFields3() throws Throwable {
    doTestInclude(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> #foo{two= <caret>}", "A", "B");
  }
  
  public void testRecordFields4() throws Throwable {
    doTestInclude(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> #foo{two=<caret>}", "A", "B");
  }  
  
  public void testRecordFields5() throws Throwable {
    doCheckResult(
      "-record(foo, {id, two}).\n" +
      "bar(A, B)-> A#foo.tw<caret>", 
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo.two");
  }

  public void testRecordFields6() throws Throwable {
    doCheckResult(
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo{tw<caret>}",
      "-record(foo, {id, two}).\n" +
        "bar(A, B)-> A#foo{two = }");
  }

  public void testMacros() throws Throwable {
    doTestInclude(
      "-define(foo, 1).\n" +
      "-define(buz, 1).\n" +
      "bar(A)-> ?<caret>", "foo", "buz");
  }

  public void testTypesInRecords() throws Throwable {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-record(rec, {id :: <caret>}).", "foo", "buz");
  }

  public void testBuiltInTypesInRecords() throws Throwable {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-record(rec, {id :: <caret>}).",
      ArrayUtilRt.toStringArray(ErlangPsiImplUtil.BUILT_IN_TYPES)
    );
  }

  public void testTypesInSpec() throws Throwable {
    doTestInclude(
      "-type foo() :: atom().\n" +
      "-type buz() :: string().\n" +
      "-spec my_fun(<caret>)", "foo", "buz", "atom", "no_return");
  }

  public void testTypesInTypeDeclaration() throws Throwable {
    doTestInclude(
      "-type foo() :: <caret>atom().\n" +
        "-type buz() :: string().\n" +
        "-type tes() :: <caret>)", "foo", "buz", "atom", "no_return");
  }

  public void testBif() throws Throwable {
    doTestInclude("foo() -> <caret>", "is_function", "is_record", "universaltime_to_posixtime");
  }

  public void testBifFromModules() throws Throwable {
    doTestInclude("foo() -> lists:<caret>", "member", "reverse", "keysearch");
  }

  public void testMultiModule() throws Throwable {
    myFixture.configureByFiles("multi-module/a.erl");
    myFixture.configureByFile("multi-module/b.erl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo", "module_info", "module_info");
    // means "bar/1", "bar/0", "foo/1", "foo/0", "module_info/0", "module_info/1"
  }

  public void testBifImport() throws Throwable {
    doTestInclude("-import(math, [<caret>]).", "sin", "sqrt");
  }

  public void testBifImport2() throws Throwable {
    doTestInclude("-import(math, [sin/1, sqrt/1]).\n" +
      "foo() -> <caret>", "sin", "sqrt");
  }

  public void test182() throws Throwable {
    doTestInclude("test() -> <caret>\n" +
      "ok.\n" +
      "my_local_function() -> not_so_ok.",
      "my_local_function");
  }

  public void testNoVariableDuplicates() throws Exception {
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

  public void testIncludeLib()  throws Exception { doCheckResult("-include_<caret>", "-include_lib(\"<caret>\")."); }
  public void testInclude()     throws Exception { doCheckResult("-inclu<caret>", "-include(\"<caret>\").", '('); }
  public void testExport()      throws Exception { doCheckResult("-exp<caret>", "-export([<caret>]).", '('); }
  public void testExportType()  throws Exception { doCheckResult("-export_t<caret>", "-export_type([<caret>])."); }
  public void testBehaviour()   throws Exception { doCheckResult("-beha<caret>", "-behaviour(<caret>)."); }

  public void testExportFunction() throws Exception {
    doCheckResult("-export([<caret>]). foo(A, B, C) -> ok.", "-export([foo/3<caret>]). foo(A, B, C) -> ok.", Lookup.COMPLETE_STATEMENT_SELECT_CHAR);
  }

  public void testLager() throws Throwable {
    doTestInclude("foo() -> lager:<caret>", "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency");
  }

  public void testImportModule() throws Throwable {
    myFixture.configureByFiles("multi-module/a.erl");
    myFixture.configureByFile("multi-module/b.erl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo", "module_info", "module_info");
    // means "bar/1", "bar/0", "foo/1", "foo/0", "module_info/0", "module_info/1"
  }

  public void testModuleCompletion() throws Throwable {
    myFixture.configureByFiles("module-completion/use_module.erl", "module-completion/test_module.erl");
    doTestVariantsInner(CompletionType.BASIC, 2, CheckType.INCLUDES, "test_module");
  }

  public void test176() throws Throwable {
    myFixture.configureByFiles("headers/a.erl", "headers/header.hrl");

    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "foo");
  }

  public void testFunctionExpression() throws Throwable {
    doCheckResult("zoo() -> fun zo<caret>", "zoo() -> fun zoo/0");
  }

  public void testFunctionExpression2() throws Throwable {
    doCheckResult("foo() -> fun <caret>", "foo() -> fun foo/0", Lookup.NORMAL_SELECT_CHAR);
  }

  public void test211() throws Throwable {
    doTestInclude("-module(test, [Id, Name::string()]). foo() -> <caret>", "Id", "Name");
  }

  public void testNoCompletionInStrings() throws Throwable {
    doTestVariants("foo() -> \"<caret>\"", CompletionType.BASIC, 1, CheckType.EQUALS);
  }

  public void testNoCompletionInComments() throws Throwable {
    doTestVariants("% <caret>", CompletionType.BASIC, 1, CheckType.EQUALS);
  }

  public void testIncludeCompletion() throws Throwable {
    localFileSystemSetUp();
    myFixture.configureByFiles("include/includeCompletion.erl", "include/include/header.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "include/");
  }

  public void testIncludeLibCompletion() throws Throwable {
    myFixture.configureByFiles("include-lib/includeLib.erl", "include-lib/testapp/ebin/testapp.app", "include-lib/testapp/include/includefile.hrl");
    myFixture.complete(CompletionType.BASIC);
    myFixture.checkResultByFile("include-lib/includeLib-after.erl");
  }

  public void testIncludeLibEmptyCompletion() throws Throwable {
    myFixture.configureByFiles("include-lib-empty/includeLib.erl",
                               "include-lib-empty/testapp/ebin/testapp.app",
                               "include-lib-empty/testapp/include/includefile.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "testapp/");
  }

  public void testSmartInteger() throws Throwable {
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

  public void testSmartCompositeTypes() throws Throwable {
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

  public void testCameCaseModules() throws Throwable {
    myFixture.configureByText("CamelCase.erl", "");
    myFixture.configureByText("a.erl", "bar() -> Cam<caret>");
    myFixture.complete(CompletionType.BASIC, 2);
    myFixture.checkResult("bar() -> 'CamelCase':<caret>");
  }

  public void testFunctionsFromCameCaseModule() throws Throwable {
    myFixture.configureByText("CamelCase.erl", "-module('CamelCase'). -export([foo/0]). foo() -> ok.");
    doTestInclude("bar() -> 'CamelCase':<caret>", "foo");
  }

  public void testOverrideInsideRecord() throws Exception {
    myFixture.configureByText("a.erl", "bar(Record, Record2) -> Rec<caret>#data{}.");
    myFixture.completeBasic();
    myFixture.type('\t');
    myFixture.checkResult("bar(Record, Record2) -> Record#data{}.");
  }
}