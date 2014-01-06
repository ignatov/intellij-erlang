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

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.impl.CodeInsightTestFixtureImpl;
import org.intellij.erlang.inspection.*;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public abstract class ErlangHighlightingTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  protected ErlangHighlightingTestBase(String platformPrefix) {
    super(platformPrefix);
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
    setUpProjectSdk();
    ((CodeInsightTestFixtureImpl)myFixture).canChangeDocumentDuringHighlighting(true);
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/";
  }

  public static void setUpInspections(@NotNull CodeInsightTestFixture fixture) {
    //noinspection unchecked
    fixture.enableInspections(
      ErlangUnboundVariableInspection.class,
      ErlangUnresolvedRecordInspection.class,
      ErlangUnresolvedRecordFieldInspection.class,
      ErlangUnresolvedExportFunctionInspection.class,
      ErlangHeadMismatchInspection.class,
      ErlangUnresolvedIncludeInspection.class,
      ErlangUnresolvedIncludeLibInspection.class,

      ErlangUnresolvedFunctionInspection.class,
      ErlangUnusedVariableInspection.class,
      ErlangUnusedFunctionInspection.class,
      ErlangDuplicateFunctionInspection.class,
      ErlangIncorrectModuleNameInspection.class,
      ErlangIoFormatInspection.class
    );
  }

  protected void doTest() {
    doTest(false);
  }

  protected void doTestWithApp() {
    doTest(true);
  }

  protected void doTest(boolean withApp) {
    String testDataFile = getTestName(false) + ".erl";
    if (withApp) {
      doTest(testDataFile, "testapp-1/ebin/testapp.app",
        "testapp-1/include/testapp.hrl", "testapp-1/include/recursive.hrl", "testapp-1/include/recursiveLib.hrl");
    }
    else {
      doTest(testDataFile);
    }
  }

  protected void doTest(String... files) {
    myFixture.configureByFiles(files);
    setUpInspections(myFixture);
    myFixture.checkHighlighting(true, false, false);
  }

  protected void enableUnresolvedMacroInspection() {
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangUnresolvedMacrosInspection.class
    );
  }
}
