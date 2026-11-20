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
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.util.List;
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

  public void testRerunGeneratorTests() {
    myFixture.configureByText("g_tests.erl", """
      -module(g_tests).
      -include_lib("eunit/include/eunit.hrl").
      plain_test() -> ok.
      gen_test_() -> [?_assert(true),
        ?_assertEqual(1, 2)].
      fixture_test_() -> {setup, fun() -> ok end,
        fun(_) -> [?_assertEqual(1, helper())] end}.
      helper() -> [?_assert(false)].
      """);

    List<SMTestProxy> failedTests = List.of(
      failedTest("-gen_test_/0-fun-1-", "g_tests:-gen_test_/0-fun-1-:5"),
      failedTest("plain_test", "g_tests:plain_test:-1"),
      failedTest("-fixture_test_/0-fun-1-", "g_tests:-fixture_test_/0-fun-1-:7"),
      failedTest("-helper/0-fun-0-", "g_tests:-helper/0-fun-0-:8"),
      failedTest("g_tests", "g_tests"));

    assertEquals(List.of("g_tests:gen_test_", "g_tests:plain_test", "g_tests:fixture_test_"),
                 List.copyOf(ErlangUnitRerunFailedTestsAction.getTestsToRerun(getProject(), failedTests)));
  }

  private static SMTestProxy failedTest(String name, String location) {
    SMTestProxy proxy = new SMTestProxy(name, false, ErlangUnitRunConfigurationType.PROTOCOL + "://" + location);
    proxy.setLocator(ErlangTestLocationProvider.INSTANCE);
    return proxy;
  }
}
