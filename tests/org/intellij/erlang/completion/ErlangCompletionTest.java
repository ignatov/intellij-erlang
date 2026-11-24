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

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.Lookup;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ArrayUtilRt;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangCompletionTest extends ErlangCompletionTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testNoUnqualifiedNotAutoImportedBifs() {
    doTestVariants("foo() -> md<caret>. md_local() -> ok.", CompletionType.BASIC, 1, CheckType.EXCLUDES,
                   "md5", "md5_init", "md5_update", "md5_final");
  }

  public void testAutoImportedBifsNotMarkedInBifTable() {
    doCheckResult("foo(P) -> port_to_l<caret>", "foo(P) -> port_to_list(<caret>)");
    doCheckResult("foo(R) -> ref_to_l<caret>", "foo(R) -> ref_to_list(<caret>)");
  }

  public void testKeywords1() { doTestInclude("-<caret>", "module", "record", "define"); }
  public void testVariablesFromDefinition() { doTestInclude("foo(A, B, C)-> <caret>", "A", "B", "C"); }
  public void testVariablesFromBody() { doTestInclude("foo(A, B, C)-> D=1, <caret>", "A", "B", "C", "D"); }

  public void testVariableFromLowercasePrefixCaseInsensitive() {
    CodeInsightSettings settings = CodeInsightSettings.getInstance();
    int oldCaseSensitive = settings.getCompletionCaseSensitive();
    settings.setCompletionCaseSensitive(CodeInsightSettings.NONE);
    try {
      doCheckResult(
        """
          start(_StartType, _StartArgs) ->
            MyName = 1,
            my<caret>""",
        """
          start(_StartType, _StartArgs) ->
            MyName = 1,
            MyName<caret>""");
    }
    finally {
      settings.setCompletionCaseSensitive(oldCaseSensitive);
    }
  }

  public void testVariablesFromLowercasePrefixCaseInsensitive() {
    doCaseInsensitiveTest(() -> doTestInclude(
      """
        foo(MyArg) ->
          MyName = 1,
          my<caret>""", "MyArg", "MyName"));
  }

  public void testNoVariablesFromLowercasePrefixInRemoteCall() {
    doCaseInsensitiveTest(() -> doTestVariants(
      """
        -module(a).
        my_one() -> ok.
        my_two() -> ok.
        foo(MyArg) ->
          MyName = 1,
          a:my<caret>""", CompletionType.BASIC, 1, CheckType.EXCLUDES, "MyArg", "MyName"));
  }

  private static void doCaseInsensitiveTest(@NotNull Runnable test) {
    CodeInsightSettings settings = CodeInsightSettings.getInstance();
    int oldCaseSensitive = settings.getCompletionCaseSensitive();
    settings.setCompletionCaseSensitive(CodeInsightSettings.NONE);
    try {
      test.run();
    }
    finally {
      settings.setCompletionCaseSensitive(oldCaseSensitive);
    }
  }

  public void testVariablesFromCaseHead() { doTestInclude("foo() -> case V = 1 of _ -> <caret> end.", "V"); }
  public void testNoFunVariablesFromCaseHead() { doTestVariants("foo() -> case (fun(Yy) -> Zz = Yy end)(1) of _ -> <caret> end.", CompletionType.BASIC, 1, CheckType.EXCLUDES, "Yy", "Zz"); }
  public void testFunctions() {
    doTestInclude(
      """
        foo() -> ok.
        buzz() -> ok.
        bar(A)-> <caret>""", "foo", "buzz");
  }

  public void testFunctionCompletionInTypedList() {
    doTestInclude("foo() -> ok. -record(state, {first = <caret>}).",
      "begin", "try", "fun", "if", "maybe",
      "node", "pid_to_list", "spawn", "binary_to_list",
      "foo");
  }

  public void testRecords() {
    doTestInclude(
      """
        -record(foo, {id}).
        -record(buz, {id}).
        bar(A)-> A#<caret>""", "foo", "buz");
  }  

  public void testNativeRecords() {
    doTestInclude(
      """
        -record #foo{id}.
        -record #buz{id}.
        bar(A)-> A#<caret>""", "foo", "buz");
  }
  
  public void testRecordFields() {
    doTestEquals(
      "-record(foo, {id, two}).\n" +
      "bar(A)-> #foo{<caret>}", "id", "two");
  }

  public void testNativeRecordFields() {
    doTestEquals(
      "-record #foo{id, two}.\n" +
      "bar(A)-> #foo{<caret>}", "id", "two");
  }
  
  public void testRecordFieldsFromTypedMacro() {
    doTestEquals(
      "-define(BASE(M), module = M :: atom(), id :: atom()).\n" +
      "-record(foo, {?BASE(m), two :: integer()}).\n" +
      "bar(A)-> #foo{<caret>}", "module", "id", "two");
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

  public void testRecordFields7() {
    doTestInclude(
      "-record(foo, {id, 'WeirdCase', 'strange-symbol'}).\n" +
      "bar(A, B)-> A#foo{<caret>}",
      "id", "'WeirdCase'", "'strange-symbol'");
  }

  public void testExportRecordAttribute() {
    doTestInclude(
      """
        -record #local{id}.
        -export_record([<caret>]).""", "local");
  }

  public void testImportRecordAttribute() {
    addNativeRecordRemoteModule();
    assertNativeRecordCompletion("-import_record(geometry, [<caret>]).");
  }

  public void testImportedNativeRecord() {
    addNativeRecordRemoteModule();
    assertNativeRecordCompletion(
      """
        -import_record(geometry, [point]).
        bar(A)-> A#<caret>""");
  }

  public void testModuleQualifiedNativeRecord() {
    addNativeRecordRemoteModule();
    assertNativeRecordCompletion("bar() -> #geometry:<caret>");
  }

  public void testImportedNativeRecordFields() {
    addNativeRecordRemoteModule();
    doTestEquals(
      """
        -import_record(geometry, [point]).
        bar() -> #point{<caret>}""", "x", "y");
  }

  public void testModuleQualifiedNativeRecordFields() {
    addNativeRecordRemoteModule();
    doTestEquals("bar() -> #geometry:point{<caret>}", "x", "y");
  }

  public void testModuleQualifiedNativeRecordFieldsFromInclude() {
    myFixture.addFileToProject("shapes.hrl", "-record #circle{radius = 0}.\n");
    myFixture.addFileToProject(
      "shapes.erl",
      """
        -module(shapes).
        -include("shapes.hrl").
        -export_record([circle]).
        """);
    doTestEquals("bar() -> #shapes:circle{<caret>}", "radius");
  }

  public void testRecordsFromInclude() {
    myFixture.addFileToProject("records.hrl", "-record(included_record, {a}).\n");
    doTestInclude("-include(\"records.hrl\").\nbar() -> #<caret>", "included_record");
  }

  public void testMacrosFromInclude() {
    myFixture.addFileToProject("macros.hrl", "-define(INCLUDED_MACRO, 1).\n");
    doTestInclude("-include(\"macros.hrl\").\nbar() -> ?<caret>", "INCLUDED_MACRO");
  }

  private void addNativeRecordRemoteModule() {
    myFixture.addFileToProject(
      "geometry.erl",
      """
        -module(geometry).

        -record #point{x = 0, y = 0}.
        -record #hidden{secret = 0}.

        -export_record([point]).
        """);
  }

  private void assertNativeRecordCompletion(@NotNull String text) {
    myFixture.configureByText("a.erl", text);
    myFixture.complete(CompletionType.BASIC, 1);
    List<String> variants = myFixture.getLookupElementStrings();
    assertNotNull(variants);
    assertTrue("Missing variant: point", variants.contains("point"));
    assertFalse("Unexpected non-exported record variant: hidden", variants.contains("hidden"));
  }

  public void testMacros() {
    doTestInclude(
      """
        -define(foo, 1).
        -define(buz, 1).
        bar(A)-> ?<caret>""", "foo", "buz");
  }

  public void testTypesInRecords() {
    doTestInclude(
      """
        -type foo() :: atom().
        -type buz() :: string().
        -record(rec, {id :: <caret>}).""", "foo", "buz");
  }

  public void testBuiltInTypesInRecords() {
    doTestInclude(
      """
        -type foo() :: atom().
        -type buz() :: string().
        -record(rec, {id :: <caret>}).""",
      ArrayUtilRt.toStringArray(ErlangPsiImplUtil.BUILT_IN_TYPES)
    );
  }

  public void testErlInternalBuiltInTypes() {
    doTestInclude(
      "-type foo() :: <caret>.",
      "arity", "binary", "bitstring", "dynamic", "function", "identifier", "iodata", "map", "nil",
      "nonempty_binary", "nonempty_bitstring", "nonempty_improper_list", "nonempty_list",
      "nonempty_maybe_improper_list", "tuple"
    );
  }

  public void testBuiltInTypeWithoutZeroArityPutsCaretInsideParentheses() {
    myFixture.configureByText("a.erl", "-type foo() :: nonempty_imp<caret>.");
    LookupElement[] elements = myFixture.completeBasic();
    assertNotNull(elements);
    myFixture.getLookup().setCurrentItem(ContainerUtil.find(elements, e -> e.getLookupString().equals("nonempty_improper_list")));
    myFixture.finishLookup(Lookup.NORMAL_SELECT_CHAR);
    myFixture.checkResult("-type foo() :: nonempty_improper_list(<caret>).");
  }

  public void testLocalTypeHidesBuiltInTypeOfSameName() {
    doCheckResult(
      """
        -type dynamic() :: term().
        -type foo() :: dyna<caret>.""",
      """
        -type dynamic() :: term().
        -type foo() :: dynamic()<caret>.""");
  }

  public void testNoBuiltInTypesInExportType() {
    doTestEquals(
      """
        -type tuple_a() :: a.
        -type tuple_b() :: b.
        -export_type([tupl<caret>]).""",
      "tuple_a", "tuple_b");
  }

  public void testLocalTypeOfOtherArityKeepsBuiltInType() {
    myFixture.configureByText("a.erl", """
      -type list(A, B) :: {A, B}.
      -type t() :: lis<caret>.""");
    LookupElement[] elements = myFixture.completeBasic();
    assertNotNull(elements);
    assertEquals(2, ContainerUtil.count(Arrays.asList(elements), e -> e.getLookupString().equals("list")));
  }

  public void testNoUndefinedOrDeprecatedBuiltInTypes() {
    doTestVariants("-type foo() :: <caret>.", CompletionType.BASIC, 1, CheckType.EXCLUDES, "improper_list", "bool");
  }

  public void testTypesInSpec() {
    doTestInclude(
      """
        -type foo() :: atom().
        -type buz() :: string().
        -spec my_fun(<caret>)""", "foo", "buz", "atom", "no_return");
  }

  public void testTypesInTypeDeclaration() {
    doTestInclude(
      """
        -type foo() :: <caret>atom().
        -type buz() :: string().
        -type tes() :: <caret>)""", "foo", "buz", "atom", "no_return");
  }

  public void testModuleQualifiedTypesInSpecResult() {
    addTypesRemoteModule();
    doTestEquals("-spec f() -> other:<caret>.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInSpecArgument() {
    addTypesRemoteModule();
    doTestEquals("-spec f(other:<caret>) -> ok.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTypeDefinition() {
    addTypesRemoteModule();
    doTestEquals("-type t() :: [other:<caret>].", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInRecordField() {
    addTypesRemoteModule();
    doTestEquals("-record(r, {a :: other:<caret>}).", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTuple() {
    addTypesRemoteModule();
    doTestEquals("-spec f() -> {ok, other:<caret>}.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTupleInUnion() {
    addTypesRemoteModule();
    doTestEquals("-spec f() -> ok | {error, other:<caret>}.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTupleInSpecArgument() {
    addTypesRemoteModule();
    doTestEquals("-spec f({ok, other:<caret>}) -> ok.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTupleInTypeDefinition() {
    addTypesRemoteModule();
    doTestEquals("-type t() :: {ok, other:<caret>}.", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesInTupleInRecordField() {
    addTypesRemoteModule();
    doTestEquals("-record(r, {a :: {ok, other:<caret>}}).", "pub", "pub_param");
  }

  public void testModuleQualifiedTypesExportedInHeader() {
    myFixture.addFileToProject("other3.hrl", """
      -export_type([h/0, h2/0]).
      -type h() :: ok.
      -type h2() :: ok.
      -type hp() :: ok.
      """);
    myFixture.addFileToProject("other3.erl", """
      -module(other3).
      -include("other3.hrl").
      """);
    doTestEquals("-spec f() -> other3:h<caret>.", "h", "h2");
  }

  public void testModuleQualifiedTypesOfOwnModuleInHeader() {
    myFixture.addFileToProject("own.erl", """
      -module(own).
      -include("own.hrl").
      """);
    myFixture.configureByText("own.hrl", """
      -type hpriv() :: ok.
      -type hpriv2() :: ok.
      -spec f() -> own:hp<caret>.""");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "hpriv", "hpriv2");
  }

  public void testModuleQualifiedTypesOfUnknownModule() {
    doTestVariants("""
      -type loc() :: ok.
      -type loc2() :: ok.
      -spec f() -> nosuch:<caret>.""", CompletionType.BASIC, 1, CheckType.EXCLUDES, "loc", "loc2");
  }

  public void testModuleQualifiedTypesOfUnknownModuleWithPrefix() {
    doTestVariants("""
      -type loc() :: ok.
      -type loc2() :: ok.
      -spec f() -> nosuch:l<caret>.""", CompletionType.BASIC, 1, CheckType.EXCLUDES, "loc", "loc2");
  }

  public void testModuleQualifiedTypesWithPrefix() {
    addTypesRemoteModule();
    doTestEquals("-type t() :: other:p<caret>", "pub", "pub_param");
  }

  public void testModuleQualifiedTypeInsertion() {
    addTypesRemoteModule();
    doCheckResult("-spec f() -> other:pub_p<caret>.", "-spec f() -> other:pub_param(<caret>).");
  }

  public void testModuleQualifiedTypesOfOwnModule() {
    myFixture.configureByText("own.erl", """
      -module(own).
      -type priv() :: integer().
      -type priv_list() :: [priv()].
      -spec f() -> own:<caret>.""");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "priv", "priv_list");
  }

  public void testModuleQualifiedTypesOfModuleMacro() {
    myFixture.configureByText("own.erl", """
      -module(own).
      -type priv() :: integer().
      -type priv_list() :: [priv()].
      -spec f() -> ?MODULE:<caret>.""");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "priv", "priv_list");
  }

  public void testModuleQualifiedTypesOfModuleMacroWithPrefix() {
    myFixture.configureByText("own.erl", """
      -module(own).
      -type priv() :: integer().
      -type priv_list() :: [priv()].
      -spec f() -> ?MODULE:p<caret>.""");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EQUALS, "priv", "priv_list");
  }

  private void addTypesRemoteModule() {
    myFixture.addFileToProject("other.erl", """
      -module(other).
      -export_type([pub/0, pub_param/1]).
      -type pub() :: atom().
      -type pub_param(T) :: [T].
      -type pub_priv() :: integer().
      """);
  }

  public void testBif() {
    doTestInclude("foo() -> <caret>", "is_function", "is_record", "tuple_size");
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
    doTestInclude("""
                    test() -> <caret>
                    ok.
                    my_local_function() -> not_so_ok.""",
                  "my_local_function");
  }

  public void testNoVariableDuplicates() {
    myFixture.configureByText("a.erl",
                              """
                                foo() ->
                                    case {1, 1} of
                                        {A, A} -> <caret>
                                    end.""");
    myFixture.complete(CompletionType.BASIC, 1);
    List<String> stringList = myFixture.getLookupElementStrings();
    assertNotNull(stringList);
    List<String> vars = ContainerUtil.filter(stringList, s -> s.equals("A"));
    assertSize(1, vars);
  }

  public void testIncludeLib()        { doCheckResult("-include_<caret>", "-include_lib(\"<caret>\")."); }
  public void testInclude()           { doCheckResult("-inclu<caret>", "-include(\"<caret>\").", '('); }
  public void testExport()            { doCheckResult("-exp<caret>", "-export([<caret>]).", '('); }
  public void testExportType()        { doCheckResult("-export_t<caret>", "-export_type([<caret>])."); }
  public void testOptionalCallbacks() { doCheckResult("-optional_c<caret>", "-optional_callbacks([<caret>])."); }
  public void testBehaviour()         { doCheckResult("-behaviou<caret>", "-behaviour(<caret>)."); }
  public void testBehavior()          { doCheckResult("-behavior<caret>", "-behavior(<caret>)."); }

  public void testAppFile() {
    doAppFileCheckResult("{ap<caret>}.", "{application}.");
  }

  public void testAppFileBadPosition() {
    doAppFileCheckResult("{application, <caret>}.", "{application, <caret>}.");
  }

  public void testAppFileParameter() {
    doAppFileCheckResult("{application, name, [{re<caret>}]}.", "{application, name, [{registered<caret>}]}.");
  }

  public void testAppFileBadParameterPosition() {
    doAppFileCheckResult("{application, aaa, [{custom, star<caret>}]}.",
                         "{application, aaa, [{custom, star<caret>}]}.");
  }

  public void testAppFileKeywordNotInTuple() {
    doAppFileCheckResult("{application, name, [<caret>]}.", "{application, name, [<caret>]}.");
  }

  public void testAppFileBadListPosition() {
    doAppFileCheckResult("{application, [{<caret>}]}.", "{application, [{<caret>}]}.");
  }

  public void testAppFileBadKeyword() {
    doAppFileCheckResult("{regis<caret>}.", "{regis<caret>}.");
  }

  public void testAppFileKeywordsAreDistinct() {
    Set<String> set = new HashSet<>(ErlangAppCompletionContributor.KEYWORDS);
    assertEquals(set.size(), ErlangAppCompletionContributor.KEYWORDS.size());
  }

  public void testSingleQuotes() {
    myFixture.configureByText("a.erl", "");
    myFixture.type("'");
    myFixture.checkResult("''");
  }

  public void testSingleQuotesWithText() {
    myFixture.configureByText("a.erl", "");
    myFixture.type("'Hello ");
    myFixture.type("'");
    myFixture.checkResult("'Hello '");
  }

  public void testExportFunction() {
    doCheckResult("-export([<caret>]). foo(A, B, C) -> ok.", "-export([foo/3<caret>]). foo(A, B, C) -> ok.", Lookup.COMPLETE_STATEMENT_SELECT_CHAR);
  }

  public void testExportFunctionStartedTyping() {
    doCheckResult("-export([f<caret>]). foo(A, B, C) -> ok.", "-export([foo/3<caret>]). foo(A, B, C) -> ok.");
  }

  public void testExportFunctionReplacesWrongArity() {
    doCheckResult("-export([fo<caret>/5]). foo(A) -> A.", "-export([foo/1<caret>]). foo(A) -> A.");
  }

  public void testExportFunctionReplacesWrongArityAfterSpace() {
    doCheckResult("-export([fo<caret> /5]). foo(A) -> A.", "-export([foo/1<caret>]). foo(A) -> A.");
  }

  public void testOptionalCallbackTupleFunction() {
    doCheckResult("-optional_callbacks([{fo<caret>, 2}]).\nfoobar(X) -> X.", "-optional_callbacks([{foobar, 1<caret>}]).\nfoobar(X) -> X.");
  }

  public void testOptionalCallbackTupleQuotedFunction() {
    doCheckResult("-optional_callbacks([{fo<caret>, 2}]).\n'foo bar'(X) -> X.", "-optional_callbacks([{'foo bar', 1<caret>}]).\n'foo bar'(X) -> X.");
  }

  public void testOptionalCallbackSlashFunction() {
    doCheckResult("-optional_callbacks([fo<caret>]).\nfoobar(X) -> X.", "-optional_callbacks([foobar/1<caret>]).\nfoobar(X) -> X.");
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

  public void testSecondBasicCompletionDoesNotOfferTypedWord() {
    doTestVariants("""
      -module(shop_tests).
      -export([all/0, small_order/0, big_order/0]).

      small_order() -> ok.
      big_order() -> ok.

      all() ->
        small_order(),
        big_order(),
        sma<caret>.""", CompletionType.BASIC, 2, CheckType.EXCLUDES, "sma");
  }

  public void testThirdBasicCompletionDoesNotOfferTypedWord() {
    doTestVariants("""
      -module(shop_tests).
      small_order() -> small.
      all() -> sma<caret>.""", CompletionType.BASIC, 3, CheckType.EXCLUDES, "sma");
  }

  public void testSecondBasicCompletionOffersTypedWordUsedElsewhereInFile() {
    doTestVariants("""
      -module(shop_tests).
      small_order() -> sma.
      all() -> sma<caret>.""", CompletionType.BASIC, 2, CheckType.INCLUDES, "sma", "small_order");
  }

  public void testSecondBasicCompletionOffersTypedWordUsedInAnotherFile() {
    myFixture.addFileToProject("other.erl", "-module(other).\nf() -> sma.");
    doTestVariants("""
      -module(shop_tests).
      small_order() -> sma<caret>.""", CompletionType.BASIC, 2, CheckType.INCLUDES, "sma", "small_order");
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
    assertEquals(compList.getFirst(), "bar");
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
    doTestInclude("foo() -> <caret>.", "fake_module", "fake_module:bar", "fake_module:tar", "group_leader");
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

  public void testNoDeletedModulesInModuleCompletion() {
    PsiFile deleted = myFixture.addFileToProject("deleted_module_1262.erl", "-module(deleted_module_1262).");
    assertTrue(ErlangModuleIndex.getNames(getProject()).contains("deleted_module_1262"));
    WriteCommandAction.runWriteCommandAction(getProject(), () -> deleted.delete());
    myFixture.configureByText("a.erl", "foo() -> deleted_mod<caret>");
    myFixture.completeBasic();
    assertEquals("foo() -> deleted_mod", myFixture.getEditor().getDocument().getText());
  }

  public void testModuleCompletionWithoutColon() {
    myFixture.configureByFiles("module-completion/test_module.erl");
    doCheckResult("foo() -> bar(test_modul<caret>", "foo() -> bar(test_module");
  }

  public void testBehaviourCompletion() {
    myFixture.configureByFiles("module-completion/behaviour_module.erl");
    doCheckResult("-behaviour(behaviour_mo<caret>).", "-behaviour(behaviour_module).");
  }

  public void testBehaviourInfoCompletion() {
    myFixture.configureByFiles("module-completion/behaviour_info_module.erl");
    doCheckResult("-behaviour(behaviour_in<caret>).", "-behaviour(behaviour_info_module).");
  }

  public void testQuotedBehaviourCompletion() {
    myFixture.configureByFiles("module-completion/quoted-behaviour.erl");
    doCheckResult("-behaviour(quo<caret>).", "-behaviour('quoted-behaviour').");
  }

  public void testBehaviourCompletionWithEndQuote() {
    myFixture.configureByFiles("module-completion/quoted-behaviour.erl");
    doCheckResult("-behaviour(quo<caret>').", "-behaviour('quoted-behaviour').");
  }

  public void testBehaviourCompletionWithBothQuotes() {
    myFixture.configureByFiles("module-completion/quoted-behaviour.erl");
    doCheckResult("-behaviour('quo<caret>').", "-behaviour('quoted-behaviour').");
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

  public void testRemoteFunctionExpressionQuotedWithStartQuote() {
    doCheckQuotedRemoteFunction("foo() -> fun mymodule:'те<caret>", "тест", "foo() -> fun mymodule:'тест'/3<caret>");
  }

  public void testRemoteFunctionExpressionQuotedWithBothQuotes() {
    doCheckQuotedRemoteFunction("foo() -> fun mymodule:'те<caret>'", "тест", "foo() -> fun mymodule:'тест'/3<caret>");
  }

  public void testRemoteFunctionExpressionQuotedWithBothQuotesAscii() {
    doCheckQuotedRemoteFunction("foo() -> fun mymodule:'dec_<caret>'", "dec_D-1", "foo() -> fun mymodule:'dec_D-1'/2<caret>");
  }

  public void testRemoteFunctionExpressionQuotedWithEndQuote() {
    doCheckQuotedRemoteFunction("foo() -> fun mymodule:dec_<caret>'", "dec_D-1", "foo() -> fun mymodule:'dec_D-1'/2<caret>");
  }

  public void testRemoteFunctionCallQuotedWithEndQuote() {
    doCheckQuotedRemoteFunction("foo() -> mymodule:dec_<caret>'", "dec_D-1", "foo() -> mymodule:'dec_D-1'(<caret>)");
  }

  public void testModuleCompletionQuotedWithEndQuote() {
    myFixture.configureByFiles("module-completion/quoted-behaviour.erl");
    doCheckResultWithItem("foo() -> quo<caret>'", "quoted-behaviour", "foo() -> 'quoted-behaviour':<caret>");
  }

  public void testRemoteFunctionExpressionPlainNameWithBothQuotes() {
    doCheckQuotedRemoteFunction("foo() -> fun mymodule:'ba<caret>'.", "bar", "foo() -> fun mymodule:'bar'/1<caret>.");
  }

  public void testRemoteFunctionCallPlainNameWithEndQuote() {
    doCheckQuotedRemoteFunction("foo() -> mymodule:ba<caret>'.", "bar", "foo() -> mymodule:'bar'(<caret>).");
  }

  public void testModuleCompletionPlainNameWithEndQuote() {
    myFixture.configureByText("plain_module.erl", "-module(plain_module).");
    doCheckResultWithItem("foo() -> plain_mod<caret>'.", "plain_module", "foo() -> 'plain_module':<caret>.");
  }

  public void testModuleCompletionBeforeQuotedAtom() {
    doCheckQuotedRemoteFunction("foo() -> mymod<caret>'dec_D-1'(1, 2).", "mymodule", "foo() -> mymodule:<caret>'dec_D-1'(1, 2).");
  }

  public void testModuleCompletionInFunExpressionBeforeQuotedAtom() {
    doCheckQuotedRemoteFunction("foo() -> fun mymod<caret>'dec_D-1'/2.", "mymodule", "foo() -> fun mymodule:<caret>'dec_D-1'/2.");
  }

  public void testRemoteFunctionCallBeforeQuotedAtom() {
    doCheckQuotedRemoteFunction("foo() -> mymodule:ba<caret>'my-atom'.", "bar", "foo() -> mymodule:bar(<caret>)'my-atom'.");
  }

  public void testLocalFunctionCallBeforeQuotedAtom() {
    doCheckResultWithItem("bar() -> ok. foo() -> ba<caret>'my-atom'.", "bar", "bar() -> ok. foo() -> bar()<caret>'my-atom'.");
  }

  public void testLocalFunctionCallAfterQuotedAtom() {
    doCheckResultWithItem("bar() -> ok. foo() -> 'a'ba<caret>", "bar", "bar() -> ok. foo() -> 'a'bar()<caret>");
  }

  public void testRemoteFunctionExpressionAfterMultiLineString() {
    doCheckQuotedRemoteFunction("foo() -> X = \"a\nit's\", fun mymodule:'dec_<caret>", "dec_D-1",
                                "foo() -> X = \"a\nit's\", fun mymodule:'dec_D-1'/2<caret>");
  }

  public void testRemoteFunctionExpressionWithBothQuotesAfterMultiLineString() {
    doCheckQuotedRemoteFunction("foo() -> X = \"a\nit's\", fun mymodule:'те<caret>'.", "тест",
                                "foo() -> X = \"a\nit's\", fun mymodule:'тест'/3<caret>.");
  }

  public void testModuleCompletionBeforeQuotedAtomAfterMultiLineString() {
    doCheckQuotedRemoteFunction("foo() -> X = \"a\nit's\", mymod<caret>'dec_D-1'(1, 2).", "mymodule",
                                "foo() -> X = \"a\nit's\", mymodule:<caret>'dec_D-1'(1, 2).");
  }

  public void testRemoteFunctionExpressionAfterMultiLineAtom() {
    doCheckQuotedRemoteFunction("foo() -> X = 'a\nb', fun mymodule:'dec_<caret>", "dec_D-1",
                                "foo() -> X = 'a\nb', fun mymodule:'dec_D-1'/2<caret>");
  }

  public void testModuleCompletionBeforeQuotedAtomAfterMultiLineAtom() {
    doCheckQuotedRemoteFunction("foo() -> X = 'a\nb', mymod<caret>'dec_D-1'(1, 2).", "mymodule",
                                "foo() -> X = 'a\nb', mymodule:<caret>'dec_D-1'(1, 2).");
  }

  private void doCheckQuotedRemoteFunction(@NotNull String before, @NotNull String item, @NotNull String after) {
    myFixture.configureByText("mymodule.erl",
                              "-module(mymodule). -export(['тест'/3, 'dec_D-1'/2, bar/1]). 'тест'(A, B, C) -> ok. 'dec_D-1'(A, B) -> ok. bar(A) -> ok.");
    doCheckResultWithItem(before, item, after);
  }

  private void doCheckResultWithItem(@NotNull String before, @NotNull String item, @NotNull String after) {
    myFixture.configureByText("a.erl", before);
    if (myFixture.completeBasic() != null) {
      LookupElement element = ContainerUtil.find(myFixture.getLookupElements(), e -> item.equals(e.getLookupString()));
      assertNotNull(element);
      myFixture.getLookup().setCurrentItem(element);
      myFixture.finishLookup(Lookup.NORMAL_SELECT_CHAR);
    }
    myFixture.checkResult(after);
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

  public void testIncludeCompletion() {
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
    doSmartTest("""
                  -spec g(A :: integer()) -> integer().
                  g(A) -> 1.
                  foo() ->
                      B = 2 / 1,
                      B2 = "",
                      B4 = (1),
                      B3 = 1 + 1*1,
                      g(<caret>);""",
                CheckType.EQUALS, "B4", "B3", "g");
  }

  public void testSmartIntegerProductWithSpaces() {
    doSmartTest("""
                  -spec g(A :: integer()) -> integer().
                  g(A) -> A.
                  foo() ->
                      B = 2 * 3,
                      C = 1,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "C");
  }

  public void testSmartIntegerSumOfProductWithSpaces() {
    doSmartTest("""
                  -spec g(A :: integer()) -> integer().
                  g(A) -> A.
                  foo() ->
                      B = 1 + 2 * 3,
                      C = 1,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "C");
  }

  public void testSmartIntegerProductWithCommentBeforeOperator() {
    doSmartTest("""
                  -spec g(A :: integer()) -> integer().
                  g(A) -> A.
                  foo() ->
                      B = 2 % two
                          * 3,
                      C = 1,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "C");
  }

  public void testSmartFloatDivisionWithSpaces() {
    doSmartTest("""
                  -spec g(A :: float()) -> float().
                  g(A) -> A.
                  foo() ->
                      B = 2 / 1,
                      C = 1.0,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "C");
  }

  public void testSmartFloatExcludesProductWithUnknownOperand() {
    doSmartTest("""
                  -spec g(A :: float()) -> float().
                  g(A) -> A.
                  foo(N) ->
                      B = N * 2,
                      C = 1.0,
                      g(<caret>);""",
                CheckType.EXCLUDES, "B");
  }

  public void testSmartFloatProductOfUnknownOperandAndFloat() {
    doSmartTest("""
                  -spec g(A :: float()) -> float().
                  g(A) -> A.
                  foo(N) ->
                      B = N*2.0,
                      B2 = N * 2.0,
                      C = 1.0,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "B2", "C");
  }

  public void testSmartFloatProductOfIntegerAndFloat() {
    doSmartTest("""
                  -spec g(A :: float()) -> float().
                  g(A) -> A.
                  foo() ->
                      B = 2 * 3.0,
                      C = 1.0,
                      g(<caret>);""",
                CheckType.INCLUDES, "B", "C");
  }

  public void testSmartCompositeTypes() {
    doSmartTest(
      """
        -spec new(Func::atom(), fun() | string()) -> integer().
        new(Func, StubFun) ->
            Str = "",
            Fun = fun () -> ok end,
            Fun2 = fun () -> ok end,
            new(atom, <caret>);
        new(Func, ClauseSpecs) -> ok.""",
      CheckType.EQUALS, "Fun", "Fun2" , "Str"
    );
  }

  public void testSmartSecondArgumentWithLowerArityOverload() {
    doSmartTest(
      """
        -spec new(atom()) -> ok.
        new(_) -> ok.
        -spec new(atom(), string()) -> ok.
        new(A, B) ->
            Str = "",
            Str2 = "",
            new(atom, <caret>).""",
      CheckType.INCLUDES, "Str", "Str2"
    );
  }

  public void testSmartFirstArgumentWithZeroArityOverload() {
    doSmartTest(
      """
        -spec foo() -> ok.
        foo() -> ok.
        -spec foo(integer()) -> ok.
        foo(I) ->
            B = 1,
            B2 = 2,
            foo(<caret>).""",
      CheckType.INCLUDES, "B", "B2"
    );
  }

  public void testSmartSecondArgumentOfBifWithLowerArityOverload() {
    myFixture.configureByText("a.erl", "foo(N) -> spawn(N, <caret>).");
    myFixture.complete(CompletionType.SMART);
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
