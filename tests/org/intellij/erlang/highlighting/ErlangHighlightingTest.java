/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.intellij.erlang.inspection.*;

public class ErlangHighlightingTest extends LightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  protected void doTest() {
    myFixture.configureByFile(getTestName(false) + ".erl");
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangUnresolvedVariableInspection.class,
      ErlangUnresolvedRecordInspection.class,
      ErlangUnresolvedFunctionInspection.class,

      ErlangUnusedVariableInspection.class,
      ErlangUnusedFunctionInspection.class
    );
    myFixture.checkHighlighting(true, false, false);
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/";
  }

  public void testHelloWorld()        { doTest(); }
  public void testExport()            { doTest(); }
//  public void testH()                 { doTest(); }
//  public void testMnesia()            { doTest(); }
  public void testIsDigits()          { doTest(); }
//  public void testDialyzerDataflow()  { doTest(); }
  public void testTest()              { doTest(); }
  public void testRecords()           { doTest(); }
//  public void testDialyzerClParse()   { doTest(); }
  public void testMp4Mux()            { doTest(); }
  public void testRecord()            { doTest(); }
  public void testQuoteUnquote()      { doTest(); }
  public void test23()                { doTest(); }
  public void testRecordsResolve()    { doTest(); }
  public void testExportResolve()     { doTest(); }
  public void testUnusedFunction()    { doTest(); }
}
