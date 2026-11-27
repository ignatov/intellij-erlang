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

package org.intellij.erlang.build;

import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunningState;
import org.intellij.erlang.jps.rebar.RebarBuilder;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.util.List;
import java.util.Set;

/**
 * Runs EUnit through the plugin with the real Erlang SDK: the precompiled reporter module must load in it.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ErlangEunitRunTest extends ErlangCompilationTestBase {
  private static final long TIMEOUT_MS = 120_000;

  public void testPassingTestIsReported() throws Exception {
    addTestFile(myModule, "demo_tests.erl", """
      -module(demo_tests).
      -include_lib("eunit/include/eunit.hrl").
      add_test() -> ?assertEqual(5, 2 + 3).
      """);
    compileAndAssertOutput(true);

    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(myProject, "demo_tests", ErlangUnitRunConfigurationType.getInstance());
    configuration.setModule(myModule);
    configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
    configuration.getConfigData().setModuleNames(Set.of("demo_tests"));

    Executor executor = DefaultRunExecutor.getRunExecutorInstance();
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder.create(executor, configuration).build();
    ProgramRunner<?> runner = environment.getRunner();
    ErlangUnitRunningState state = configuration.getState(executor, environment);
    assertNotNull(state);

    ExecutionResult result = state.execute(executor, runner);
    Disposer.register(getTestRootDisposable(), result.getExecutionConsole());
    ProcessHandler processHandler = result.getProcessHandler();
    StringBuilder output = new StringBuilder();
    processHandler.addProcessListener(new ProcessAdapter() {
      @Override
      public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
        synchronized (output) {
          output.append(event.getText());
        }
      }
    });
    processHandler.startNotify();
    assertTrue("EUnit did not finish in time", processHandler.waitFor(60_000));
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();

    String text;
    synchronized (output) {
      text = output.toString();
    }
    assertEquals(text, Integer.valueOf(0), processHandler.getExitCode());
    SMTestProxy.SMRootTestProxy root = ((SMTRunnerConsoleView) result.getExecutionConsole()).getResultsViewer().getTestsRootNode();
    List<? extends SMTestProxy> tests = ContainerUtil.filter(root.getAllTests(), SMTestProxy::isLeaf);
    assertEquals(text, List.of("add_test"), ContainerUtil.map(tests, SMTestProxy::getName));
    assertTrue(text, tests.get(0).isPassed());
  }

  // The reporter for older releases was compiled with OTP 22, and OTP 27 and later refuse to load it, so eunit failed
  // with undef on eunit_teamcity:start/1.
  public void testReporterReportsPassedAndFailedTests() throws Exception {
    addTestFile(myModule, "sample_tests.erl",
                "-module(sample_tests).\n" +
                "-include_lib(\"eunit/include/eunit.hrl\").\n" +
                "passing_test() -> ?assertEqual(2, 1 + 1).\n" +
                "failing_test() -> ?assertEqual(3, list_to_integer(\"2\")).\n");
    compileAndAssertOutput(true);

    String output = runEunit("sample_tests");
    assertTrue(output, output.contains("##teamcity[testSuiteStarted name='sample_tests'"));
    assertTrue(output, output.contains("##teamcity[testFinished name='passing_test'"));
    assertTrue(output, output.contains("##teamcity[testFailed name='failing_test'"));
    assertFalse(output, output.contains("##teamcity[testFailed name='passing_test'"));
  }

  // #711: with "Compile project with rebar" turned on, the build runs only `rebar3 compile`, which does not compile the
  // test sources, so EUnit aborted with {module_not_found,tests}. Needs the rebar3 escript in rebar.path.
  public void testTestModuleOfRebarProject() throws Exception {
    String rebarPath = RebarBuilder.getTestsRebarPath();
    Assume.assumeTrue("Set the rebar.path system property to the rebar3 escript to run this test", rebarPath != null);
    ErlangCompilerSettings.getInstance(myProject).setUseRebarCompilerEnabled(true);
    RebarSettings.getInstance(myProject).setRebarPath(rebarPath);
    VirtualFile[] contentRoots = ModuleRootManager.getInstance(myModule).getContentRoots();
    assertSize(1, contentRoots);
    addFileToDirectory(contentRoots[0], "rebar.config", "{erl_opts, [debug_info]}.\n");
    addSourceFile(myModule, "app1.app.src", "{application, app1, [{vsn, \"1.0.0\"}, {applications, [kernel, stdlib]}]}.\n");
    addSourceFile(myModule, "app1_mod.erl", "-module(app1_mod).\n-export([v/0]).\nv() -> 1.\n");
    addTestFile(myModule, "tests.erl",
                "-module(tests).\n" +
                "-include_lib(\"eunit/include/eunit.hrl\").\n" +
                "v_test() -> ?assertEqual(1, app1_mod:v()).\n");
    myCompilationRunner.compile();

    String output = runEunit("tests");
    assertTrue(output, output.contains("##teamcity[testFinished name='v_test'"));
    assertFalse(output, output.contains("##teamcity[testFailed"));
  }

  private String runEunit(String moduleName) throws Exception {
    ErlangUnitRunConfigurationType type = ErlangUnitRunConfigurationType.getInstance();
    ErlangUnitRunConfiguration configuration = new ErlangUnitRunConfiguration(myProject, "eunit", type);
    configuration.setModule(myModule);
    configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
    configuration.getConfigData().setModuleNames(Set.of(moduleName));

    Executor executor = DefaultRunExecutor.getRunExecutorInstance();
    ProgramRunner<?> runner = ProgramRunner.findRunnerById("ErlangRunner");
    assertNotNull(runner);
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder.create(myProject, executor, configuration).runner(runner).build();
    RunProfileState state = configuration.getState(executor, environment);
    assertNotNull(state);
    ExecutionResult result = state.execute(executor, runner);
    assertNotNull(result);
    if (result.getExecutionConsole() != null) {
      Disposer.register(getTestRootDisposable(), result.getExecutionConsole());
    }

    ProcessHandler processHandler = result.getProcessHandler();
    StringBuilder output = new StringBuilder();
    processHandler.addProcessListener(new ProcessAdapter() {
      @Override
      public void onTextAvailable(ProcessEvent event, Key outputType) {
        synchronized (output) {
          output.append(event.getText());
        }
      }
    });
    processHandler.startNotify();
    assertTrue("eunit did not finish in time", processHandler.waitFor(TIMEOUT_MS));
    synchronized (output) {
      return output.toString();
    }
  }
}
