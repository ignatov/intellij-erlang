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
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Key;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;

import java.util.Set;

/**
 * Runs an EUnit run configuration with the Erlang of the erlang.sdk.path system property.
 */
public class ErlangEunitRunTest extends ErlangCompilationTestBase {
  private static final long TIMEOUT_MS = 120_000;

  // The reporter is loaded by the Erlang that runs the tests. The .beam that used to be bundled was compiled with
  // OTP 22, and OTP 27 and later refuse to load it, so eunit failed with undef on eunit_teamcity:start/1.
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
