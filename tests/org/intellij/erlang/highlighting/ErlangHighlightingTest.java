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
  public void testExportResolve()     { doTest(); }
  public void testUnusedFunction()    { doTest(); }
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
  public void test190()               { doTest(); }
  public void testMacrosResolve()     { doTest(); }
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
  public void testUnresolvedFunction(){ doTest(); }
  public void test354()               { doTest(); }
  public void test364()               { doTest(); }
  public void test365()               { doTest(); }
  public void test387()               { doTest(); }
  public void testMultiTarget()       { doTest(); }
  public void testInFunClause()       { doTest(); }
  public void testDuplicateExport1()  { doTest(); }
  public void testDuplicateExport2()  { doTest(); }
  public void testDefineImported1()   { doTest(); }
  public void testDefineImported2()   { doTest(); }

  public void testAutoimportCall1()   { doTest(); }
  public void testAutoimportCall2()   { doTest(); }
  public void testAutoimportCall3()   { doTest(); }

  public void testNoAutoImport1()     { doTest(); }
  public void testNoAutoImport2()     { doTest(); }
  public void testNoAutoImport3()     { doTest(); }
  public void testNoAutoImport4()     { doTest(); }
  public void testNoAutoImport5()     { doTest(); }
  public void testNoAutoImport6()     { doTest(); }
  public void testNoAutoImport7()     { doTest(); }
  public void test605()               { doTest(); }

  private void doTestWithInclude() {
    myFixture.configureByText("incl.erl",
      "-module(incl).\n" +
        "-export([crc32/1, abs/1, dt_get_tag/0, bar/0, abs/0]).\n" +
        "\n" +
        "crc32(Data) -> Data.\n" +
        "abs(D) -> D.\n" +
        "abs() -> zero.\n" +
        "dt_get_tag() -> ok.\n" +
        "bar() -> ok.");
    doTest();
  }

  public void testImportAutoimported()    { doTestWithInclude(); }

  public void testErlang17SyntaxError() {
    enableErlang17SyntaxInspection();
    doTest();
  }

  public void testErlang18SyntaxError() {
    enableErlang18SyntaxInspection();
    doTest();
  }

  public void testUnresolvedMacros()  {
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
}
