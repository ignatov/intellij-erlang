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

package org.intellij.erlang.eunit;

import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.io.File;
import java.util.Map;
import java.util.Set;

public class ErlangUnitRerunFailedTestsActionTest extends BasePlatformTestCase {
  public void testRerunKeepsEnvironment() {
    EnvironmentVariablesData envData = EnvironmentVariablesData.create(Map.of("ERL_LIBS", "/opt/x"), false);
    ErlangUnitRunConfiguration original =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    original.setEnvData(envData);

    ErlangUnitRunConfiguration rerun =
      ErlangUnitRerunFailedTestsAction.createRerunConfiguration(original, Set.of("sample_tests:failing_test"));

    assertEquals(envData, rerun.getEnvData());
    assertEquals(envData, new ErlangUnitRunningState(null, getModule(), rerun).getEnvData());
    assertEquals(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION, rerun.getConfigData().getKind());
    assertEquals(Set.of("sample_tests:failing_test"), rerun.getConfigData().getFunctionNames());
  }

  public void testRerunKeepsWorkDirectory() {
    ErlangUnitRunConfiguration original =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    original.setWorkDirectory("/tmp/wd");

    ErlangUnitRunConfiguration rerun =
      ErlangUnitRerunFailedTestsAction.createRerunConfiguration(original, Set.of("sample_tests:failing_test"));

    ErlangUnitRunningState state = new ErlangUnitRunningState(null, getModule(), rerun);
    GeneralCommandLine commandLine = new GeneralCommandLine();
    state.setWorkDirectory(commandLine);

    assertEquals("/tmp/wd", state.getWorkDirectory());
    assertEquals(new File("/tmp/wd"), commandLine.getWorkDirectory());
  }
}
