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
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageManagerImpl;
import com.intellij.testFramework.UsefulTestCase;
import com.intellij.testFramework.fixtures.IdeaProjectTestFixture;
import com.intellij.testFramework.fixtures.IdeaTestFixtureFactory;
import com.intellij.testFramework.fixtures.TestFixtureBuilder;
import com.intellij.testFramework.fixtures.impl.TempDirTestFixtureImpl;
import com.intellij.util.ArrayUtilRt;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangCompletionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private enum CheckType { EQUALS, INCLUDES, EXCLUDES }

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  protected String getTestDataPath() {
    return "testData/completion/";
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
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo"); // means "bar/1", "bar/0", "foo/1", "foo/0"
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
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "bar", "bar", "foo", "foo"); // means "bar/1", "bar/0", "foo/1", "foo/0"
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
    doCheckResult("foo() -> fun f<caret>", "foo() -> fun foo/0");
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

  private void localFileSystemSetUp() throws Exception {
    IdeaTestFixtureFactory factory = IdeaTestFixtureFactory.getFixtureFactory();
    TestFixtureBuilder<IdeaProjectTestFixture> fixtureBuilder = factory.createLightFixtureBuilder(getProjectDescriptor());

    final IdeaProjectTestFixture fixture = fixtureBuilder.getFixture();
    myFixture = IdeaTestFixtureFactory.getFixtureFactory().createCodeInsightFixture(fixture, new TempDirTestFixtureImpl());

    InjectedLanguageManagerImpl.checkInjectorsAreDisposed(getProject());
    myFixture.setUp();
    myFixture.setTestDataPath(getTestDataPath());
    myModule = myFixture.getModule();
  }
  
  private void doCheckResult(@NotNull String before, @NotNull String after) { doCheckResult(before, after, null); }
  
  private void doCheckResult(@NotNull String before, @NotNull String after, @Nullable Character c) {
    myFixture.configureByText("a.erl", before);
    myFixture.completeBasic();
    if (c != null) myFixture.type(c);
    myFixture.checkResult(after);
  }

  public void testIncludeCompletion() throws Throwable {
    localFileSystemSetUp();
    myFixture.configureByFiles("include/includeCompletion.erl", "include/include/header.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include/");
  }

  public void testIncludeLibCompletion() throws Throwable {
    myFixture.configureByFiles("include-lib/includeLib.erl", "include-lib/testapp/ebin/testapp.app", "include-lib/testapp/include/includefile.hrl");
    myFixture.complete(CompletionType.BASIC);
    myFixture.checkResultByFile("include-lib/includeLib-after.erl");
  }

  private void doTestInclude(String txt, String... variants) throws Throwable {
    doTestVariants(txt, CompletionType.BASIC, 1, CheckType.INCLUDES, variants);
  }

  protected void doTestVariants(String txt, CompletionType type, int count, CheckType checkType, String... variants) throws Throwable {
    myFixture.configureByText("a.erl", txt);
    doTestVariantsInner(type, count, checkType, variants);
  }

  protected void doTestVariantsInner(CompletionType type, int count, CheckType checkType, String... variants) throws Throwable {
    myFixture.complete(type, count);
    List<String> stringList = myFixture.getLookupElementStrings();
    assertNotNull(stringList);
    Collection<String> varList = new ArrayList<String>(Arrays.asList(variants));
    if (checkType == CheckType.EQUALS) {
      UsefulTestCase.assertSameElements(stringList, variants);
    }
    else if (checkType == CheckType.INCLUDES) {
      varList.removeAll(stringList);
      assertTrue("Missing variants: " + varList, varList.isEmpty());
    }
    else if (checkType == CheckType.EXCLUDES) {
      varList.retainAll(stringList);
      assertTrue("Unexpected variants: " + varList, varList.isEmpty());
    }
  }
}