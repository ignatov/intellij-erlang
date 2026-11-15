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
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.sm.runner.SMTestProxy;
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunningState;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

/**
 * Runs EUnit through the plugin with the real Erlang SDK: the precompiled reporter module must load in it.
 */
public class ErlangEunitRunTest extends ErlangCompilationTestBase {
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
}
