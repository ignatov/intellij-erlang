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

package org.intellij.erlang.runconfig;

import com.intellij.execution.ExecutionException;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangRunConfigurationArgumentsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  public void testApplicationErlFlagWithQuotedSpace() throws ExecutionException {
    assertErlFlags("-pa \"/Users/John Smith/ebin\" +P 1000", "-pa", "/Users/John Smith/ebin", "+P", "1000");
  }

  public void testApplicationErlFlagsAreSplitOnSpaces() throws ExecutionException {
    assertErlFlags("+P 1000  -sname node", "+P", "1000", "-sname", "node");
  }

  private void assertErlFlags(String erlFlags, String... expected) throws ExecutionException {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModuleAndFunction("sample start");
    configuration.setErlFlags(erlFlags);

    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    List<String> parameters = state.getCommand().getParametersList().getList();
    // The erl flags are the last parameters of the command line.
    assertEquals(List.of(expected), parameters.subList(parameters.size() - expected.length, parameters.size()));
    assertEquals("-noshell", parameters.get(parameters.size() - expected.length - 1));
  }
}
