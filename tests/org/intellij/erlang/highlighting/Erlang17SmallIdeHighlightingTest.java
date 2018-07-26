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

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkForSmallIdes;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class Erlang17SmallIdeHighlightingTest extends ErlangHighlightingTestBase {
  private static final String SDK_HOME = "testData/mockSdk-R17";

  public Erlang17SmallIdeHighlightingTest() {
    super(true);
  }

  @Nullable
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_17_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    // we create an sdk here so that we have sdk version cached
    ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_17_0);
    ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), SDK_HOME);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), "");
    }
    finally {
      super.tearDown();
    }
  }

  public void testCorrectSdkReleaseIsReported() {
    assertEquals(ErlangSdkRelease.V_17_0.getOtpRelease(), Objects.requireNonNull(ErlangSdkType.getRelease(getProject())).getOtpRelease());
  }

  public void testErlang17SyntaxNoError() {
    enableErlang17SyntaxInspection();
    doTest();
  }
}
