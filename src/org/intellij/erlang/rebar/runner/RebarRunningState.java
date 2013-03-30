/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderImpl;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.testframework.autotest.ToggleAutoTestAction;
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.console.FileReferenceFilter;
import org.intellij.erlang.eunit.ErlangTestLocationProvider;
import org.intellij.erlang.eunit.ErlangUnitConsoleProperties;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;

import java.util.List;

final class RebarRunningState extends CommandLineState {
  private final RebarRunConfiguration myConfiguration;

  public RebarRunningState(@NotNull final ExecutionEnvironment env, @NotNull final RebarRunConfiguration config) {
    super(env);
    myConfiguration = config;
  }

  @NotNull
  @Override
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
    if (!myConfiguration.isUseTestConsole()) {
      final TextConsoleBuilder consoleBuilder = new TextConsoleBuilderImpl(myConfiguration.getProject()) {
        @Override
        public ConsoleView getConsole() {
          final ConsoleView consoleView = super.getConsole();
          ErlangConsoleUtil.attachFilters(myConfiguration.getProject(), consoleView);
          return consoleView;
        }
      };
      setConsoleBuilder(consoleBuilder);
      return super.execute(executor, runner);
    }
    else {
      ProcessHandler processHandler = startProcess();
      setConsoleBuilder(getConsoleBuilder());

      ConsoleView consoleView = createConsoleView(executor);
      ErlangConsoleUtil.attachFilters(myConfiguration.getProject(), consoleView);
      consoleView.attachToProcess(processHandler);

      DefaultExecutionResult executionResult = new DefaultExecutionResult(consoleView, processHandler);
      executionResult.setRestartActions(new ToggleAutoTestAction());
      return executionResult;
    }
  }

  @NotNull
  private ConsoleView createConsoleView(Executor executor) throws ExecutionException {
    final RebarRunConfiguration runConfiguration = (RebarRunConfiguration) getRunnerSettings().getRunProfile();
    ErlangUnitConsoleProperties consoleProperties = new ErlangUnitConsoleProperties(runConfiguration, executor, false);
    
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH));
    consoleProperties.addStackTraceFilter(new FileReferenceFilter(myConfiguration.getProject(), ErlangConsoleUtil.EUNIT_FAILURE_PATH));
    
    return SMTestRunnerConnectionUtil.createConsoleWithCustomLocator(
      "Rebar",
      consoleProperties,
      getRunnerSettings(),
      getConfigurationSettings(),
      new ErlangTestLocationProvider() // todo: new provider?
    );
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    final RebarSettings rebarSettings = RebarSettings.getInstance(myConfiguration.getProject());
    commandLine.setWorkDirectory(myConfiguration.getProject().getBasePath());
    commandLine.setExePath(rebarSettings.getRebarPath());
    
    List<String> split = ContainerUtil.list(myConfiguration.getCommand().split("\\s+"));
    if (myConfiguration.isSkipDependencies() && !split.contains("skip_deps=true")) {
      commandLine.addParameter("skip_deps=true");        
    }
    commandLine.addParameters(split);
    
    if (myConfiguration.isUseTestConsole() && !split.contains("--verbose")) {
      commandLine.addParameter("--verbose");
    }
    
    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }
}
