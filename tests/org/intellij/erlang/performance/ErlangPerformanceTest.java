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

package org.intellij.erlang.performance;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.highlighting.ErlangHighlightingTestBase;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangPerformanceTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/performance/";
  }

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
    ErlangHighlightingTestBase.setUpInspections(myFixture);
    setUpProjectSdk();
  }

  private void doTest(int expectedMs) {
    PlatformTestUtil.newBenchmark("erlang highlighting is slow", () -> {
      myFixture.configureByFile(getTestName(false) + ".erl");
      myFixture.doHighlighting();
    }).attempts(100).start();
  }

  public void testConditionalBranches() {
    StringBuilder source = new StringBuilder("-module(conditional_branches).\n");
    for (int i = 0; i < 1000; i++) {
      source.append("-ifdef(FEATURE_").append(i).append(").\n")
        .append("function_").append(i).append("() -> ok.\n")
        .append("-else.\n")
        .append("function_").append(i).append("() -> ok.\n")
        .append("-endif.\n");
    }

    myFixture.configureByText("ConditionalBranches.erl", source.toString());
    myFixture.doHighlighting();
    Document document = myFixture.getEditor().getDocument();
    long start = System.nanoTime();
    for (int i = 0; i < 5; i++) {
      String functionName = "timed_" + i;
      String addition = "-ifdef(TIMED_" + i + ").\n" + functionName + "() -> ok.\n" +
                        "-else.\n" + functionName + "() -> ok.\n" + "-endif.\n";
      WriteCommandAction.runWriteCommandAction(getProject(), () -> document.insertString(document.getTextLength(), addition));
      PsiDocumentManager.getInstance(getProject()).commitDocument(document);
      boolean unusedFunctionFound = false;
      boolean duplicateFunctionFound = false;
      for (HighlightInfo info : myFixture.doHighlighting()) {
        String description = info.getDescription();
        if (("Unused function '" + functionName + "/0'").equals(description)) unusedFunctionFound = true;
        if (("Duplicate function '" + functionName + "/0'").equals(description)) duplicateFunctionFound = true;
      }
      assertTrue("Missing unused function highlight for " + functionName, unusedFunctionFound);
      assertFalse("Exclusive definitions reported as duplicate for " + functionName, duplicateFunctionFound);
    }
    long elapsedMillis = (System.nanoTime() - start) / 1_000_000;
    assertTrue("Conditional branch highlighting took " + elapsedMillis + " ms", elapsedMillis < 60_000);
  }

//  public void testDialyzerDataflow() { doTest(5000); }
}
