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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.intellij.erlang.inspection.*;
import org.intellij.erlang.sdk.ErlangSdkType;

public class ErlangHighlightingTest extends LightPlatformCodeInsightFixtureTestCase {
  protected void doTest() {
    myFixture.configureByFile(getTestName(false) + ".erl");
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangUnboundVariableInspection.class,
      ErlangUnresolvedRecordInspection.class,
      ErlangUnresolvedRecordFieldInspection.class,
      ErlangUnresolvedExportFunctionInspection.class,
      ErlangHeadMismatchInspection.class,

      ErlangUnresolvedFunctionInspection.class,
      ErlangUnusedVariableInspection.class,
      ErlangUnusedFunctionInspection.class,
      ErlangDuplicateFunctionInspection.class,
      ErlangIncorrectModuleNameInspection.class,
      ErlangIoFormatInspection.class
    );
    myFixture.checkHighlighting(true, false, false);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        Sdk sdk = getProjectDescriptor().getSdk();
        ProjectJdkTable.getInstance().addJdk(sdk);
        ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
      }
    });
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

  public void testUnresolvedMacros()  {
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangUnresolvedMacrosInspection.class
    );
    doTest();
  }

  public void test176() {
    myFixture.configureByText("aaa.hrl", "foo() -> ok.");
  }
}
