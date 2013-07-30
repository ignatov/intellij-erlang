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

package org.intellij.erlang.console;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderImpl;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessTerminatedListener;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangConsoleCommandLineState extends CommandLineState {
  @NotNull private final ErlangConsoleRunConfiguration myConfig;

  public ErlangConsoleCommandLineState(@NotNull ErlangConsoleRunConfiguration config,
                                       @NotNull ExecutionEnvironment env) {
    super(env);
    myConfig = config;
    TextConsoleBuilder consoleBuilder = new TextConsoleBuilderImpl(myConfig.getProject()) {
      @Override
      public ConsoleView getConsole() {
        ErlangConsoleView consoleView = new ErlangConsoleView(myConfig.getProject());
        ErlangConsoleUtil.attachFilters(myConfig.getProject(), consoleView);
        return consoleView;
      }
    };
    setConsoleBuilder(consoleBuilder);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    Project project = myConfig.getProject();
    Module module = myConfig.getConfigurationModule().getModule();
    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setExePath(ErlangConsoleUtil.getErlPath(project, module));
    String consoleArgs = myConfig.getConsoleArgs();
    commandLine.addParameters(StringUtil.split(consoleArgs, " "));
    commandLine.addParameters(ErlangConsoleUtil.getCodePath(project, module, false));
    commandLine.setWorkDirectory(ErlangConsoleUtil.getWorkingDirPath(project, myConfig.getWorkingDirPath()));
    OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
    ProcessTerminatedListener.attach(handler);
    return handler;
  }
}
