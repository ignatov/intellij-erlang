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

package org.intellij.erlang.console;

import com.intellij.execution.ExecutionException;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangConsoleCommandLineStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  public void testConsoleArgWithQuotedSpace() throws ExecutionException {
    assertConsoleArgs("-pa \"C:\\Program Files\\app\\ebin\" -sname node",
                      "-pa", "C:\\Program Files\\app\\ebin", "-sname", "node");
  }

  public void testConsoleArgsAreSplitOnSpaces() throws ExecutionException {
    assertConsoleArgs("-sname node  +P 1000", "-sname", "node", "+P", "1000");
  }

  private void assertConsoleArgs(String consoleArgs, String... expected) throws ExecutionException {
    ErlangConsoleRunConfiguration configuration = new ErlangConsoleRunConfiguration("console", getProject());
    configuration.getConfigurationModule().setModule(getModule());
    configuration.setConsoleArgs(consoleArgs);

    List<String> parameters = ErlangConsoleCommandLineState.createCommandLine(configuration).getParametersList().getList();
    // The console arguments are the first parameters of the command line, the code path follows them.
    assertEquals(List.of(expected), parameters.subList(0, expected.length));
    assertEquals(ErlangConsoleUtil.getCodePath(getProject(), getModule(), false),
                 parameters.subList(expected.length, parameters.size()));
  }
}
