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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderImpl;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.ui.ConsoleView;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.jetbrains.annotations.NotNull;

final class RebarRunningState extends CommandLineState {
  private final RebarRunConfigurationBase myConfiguration;

  public RebarRunningState(@NotNull final ExecutionEnvironment env, @NotNull final RebarRunConfigurationBase config) {
    super(env);
    myConfiguration = config;
  }

  @NotNull
  @Override
  public ExecutionResult execute(@NotNull Executor executor, @NotNull ProgramRunner runner) throws ExecutionException {
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

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    GeneralCommandLine commandLine = RebarRunningStateUtil.getRebarCommandLine(myConfiguration);
    return RebarRunningStateUtil.runRebar(myConfiguration.getProject(), commandLine);
  }
}
