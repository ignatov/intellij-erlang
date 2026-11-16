/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class RebarRunningStateUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  public void testCommandArgumentWithQuotedSpace() {
    assertRebarCommand("ct --dir \"/Users/John Smith/test\" --verbose", "ct", "--dir", "/Users/John Smith/test", "--verbose");
  }

  public void testCommandIsSplitOnSpaces() {
    assertRebarCommand("eunit  --module=sample_tests", "eunit", "--module=sample_tests");
  }

  private void assertRebarCommand(String command, String... expected) {
    RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
    configuration.setCommand(command);

    List<String> parameters = RebarRunningStateUtil.getRebarCommandLine(configuration).getParametersList().getList();
    // The rebar escript is the first parameter, the rebar command follows it.
    assertEquals(RebarSettings.getInstance(getProject()).getRebarPath(), parameters.get(0));
    assertEquals(List.of(expected), parameters.subList(1, parameters.size()));
  }
}
