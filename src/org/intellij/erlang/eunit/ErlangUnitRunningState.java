/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.autotest.ToggleAutoTestAction;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.runner.ErlangApplicationConfiguration;
import org.intellij.erlang.runner.ErlangRunningState;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangUnitRunningState extends ErlangRunningState {
  public ErlangUnitRunningState(ExecutionEnvironment env, Module module, ErlangApplicationConfiguration configuration) {
    super(env, module, configuration);
  }

  @Override
  protected void setUpParameters(GeneralCommandLine commandLine) {
    commandLine.addParameter("-run");
    commandLine.addParameter("-eval");
    commandLine.addParameter("eunit:test([" + myConfiguration.getModuleAndFunction() + "], [verbose]).");
    commandLine.addParameters("-s", "init", "stop", "-noshell");
  }

  @Override
  @NotNull
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
    ProcessHandler processHandler = startProcess();
    setConsoleBuilder(getConsoleBuilder());

    ConsoleView consoleView = createConsoleView(executor);
    ErlangConsoleUtil.attachFilters(myConfiguration.getProject(), consoleView);
    consoleView.attachToProcess(processHandler);

    DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);
    executionResult.setRestartActions(new ToggleAutoTestAction());
    return executionResult;
  }

  @NotNull
  private ConsoleView createConsoleView(Executor executor) throws ExecutionException {
    final ErlangUnitRunConfiguration runConfiguration = (ErlangUnitRunConfiguration) getRunnerSettings().getRunProfile();
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator(
      "Erlang",
      new ErlangUnitConsoleProperties(runConfiguration, executor, true),
      getRunnerSettings(),
      getConfigurationSettings(),
      new ErlangTestLocationProvider()
    );
  }
}
