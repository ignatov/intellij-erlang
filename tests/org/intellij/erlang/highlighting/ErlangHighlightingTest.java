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

package org.intellij.erlang.highlighting;

import com.intellij.util.PlatformUtilsCore;

public class ErlangHighlightingTest extends ErlangHighlightingTestBase {
  public ErlangHighlightingTest() {
    super(PlatformUtilsCore.COMMUNITY_PREFIX);
  }

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
  public void testUnresolvedFunction(){ doTest(); }
  public void test354()               { doTest(); }
  public void test364()               { doTest(); }
  public void test365()               { doTest(); }
  public void test387()               { doTest(); }

  public void testUnresolvedMacros()  {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void test176() {                    
    myFixture.configureByText("aaa.hrl", "foo() -> ok.");
    doTest();
  }

  public void testIncludeResolve()             { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testIncludeLibResolve()          { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRecursiveIncludeResolve()    { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRecursiveIncludeLibResolve() { enableUnresolvedMacroInspection(); doTestWithApp(); }
  public void testRelativePathInclude()  { doTestWithApp(); }
}
