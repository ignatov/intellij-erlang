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
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.openapi.util.Disposer;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.eunit.ErlangTestLocationProvider;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.rebar.settings.RebarSettings;

import java.util.ArrayList;
import java.util.List;
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

  private static final String A = "-module(a).\n-include_lib(\"eunit/include/eunit.hrl\").\na1_test() -> ok.\na2_test() -> ?assert(false).\n";
  private static final String C = """
    -module(c).
    -include_lib("eunit/include/eunit.hrl").
    c4_test_() -> [fun fhelper/0, {with, 1, [fun with1/1]}, ?_assert(false)].
    fhelper() -> error(bad).
    with1(_) -> error(bad).
    """;

  public void testRebar3RerunsModuleOfFailedFunctionThatIsNotATest() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("c.erl", C);
    List<ErlangFunction> failedGeneratedTests = new ArrayList<>();
    String command = createRerunCommand(failedGeneratedTests, "eunit://c:fhelper:-1", "eunit://c:with1:-1", "eunit://a:a2_test");
    assertEquals("eunit --module=c --test=a:a2_test", command);
    assertEquals(List.of("with1"), List.of(failedGeneratedTests.getFirst().getName()));
  }

  public void testRebar3RerunsModuleOfFailedFunOfGenerator() {
    useRebar("rebar3");
    myFixture.addFileToProject("a.erl", A);
    myFixture.addFileToProject("c.erl", C);
    assertEquals("eunit --module=c --test=a:a2_test",
                 createRerunCommand(new ArrayList<>(), "eunit://c:-c4_test_/0-fun-2-:3", "eunit://a:a2_test"));
  }

  public void testRebar2RerunCommandUnchanged() {
    useRebar("rebar");
    myFixture.addFileToProject("a.erl", A);
    assertEquals("eunit suites=a tests=a2_test", createRerunCommand(new ArrayList<>(), "eunit://a:a2_test"));
  }

  private String createRerunCommand(List<ErlangFunction> failedGeneratedTests, String... locationUrls) {
    List<SMTestProxy> proxies = new ArrayList<>();
    for (String url : locationUrls) {
      SMTestProxy proxy = new SMTestProxy(url, false, url);
      proxy.setLocator(ErlangTestLocationProvider.INSTANCE);
      proxies.add(proxy);
    }
    return RebarEunitRerunFailedTestsAction.createRerunCommand(getProject(), proxies, failedGeneratedTests);
  }

  private void useRebar(String rebarName) {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    rebarSettings.setRebarPath("/opt/rebar/" + rebarName);
    Disposer.register(getTestRootDisposable(), () -> rebarSettings.setRebarPath(oldRebarPath));
  }
}
