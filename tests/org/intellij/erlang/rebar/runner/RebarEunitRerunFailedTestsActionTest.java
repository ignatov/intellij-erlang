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

import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.util.Map;

public class RebarEunitRerunFailedTestsActionTest extends BasePlatformTestCase {
  public void testRerunKeepsEnvironment() {
    EnvironmentVariablesData envData = EnvironmentVariablesData.create(Map.of("ERL_LIBS", "/opt/x"), false);
    RebarEunitRunConfiguration original = new RebarEunitRunConfiguration(getProject(), "eunit");
    original.setEnvData(envData);
    original.setModule(getModule());

    RebarEunitRunConfiguration rerun = RebarEunitRerunFailedTestsAction.createRerunConfiguration(original, "eunit --module=sample_tests");

    assertEquals(envData, rerun.getEnvData());
    assertEquals(getModule(), rerun.getConfigurationModule().getModule());
    assertEquals("eunit --module=sample_tests", rerun.getCommand());
    assertTrue(rerun.isSkipDependencies());
  }
}
