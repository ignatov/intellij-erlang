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

package org.intellij.erlang.performance;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ThrowableRunnable;
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
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    ErlangHighlightingTestBase.setUpInspections(myFixture);
    setUpProjectSdk();
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  protected void doTest(int expectedMs) {
    PlatformTestUtil.startPerformanceTest("erlang highlighting is slow", expectedMs, new ThrowableRunnable() {
      @Override
      public void run() throws Throwable {
        myFixture.configureByFile(getTestName(false) + ".erl");
        myFixture.doHighlighting();
      }
    }).attempts(100).cpuBound().usesAllCPUCores().assertTiming();
  }

  public void testDialyzerDataflow() throws Exception { doTest(5000); }
}
