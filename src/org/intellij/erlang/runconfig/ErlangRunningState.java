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

package org.intellij.erlang.runconfig;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

/**
 * @author savenko
 */
public abstract class ErlangRunningState extends CommandLineState {
  private final Module myModule;

  public ErlangRunningState(ExecutionEnvironment env, Module module) {
    super(env);
    this.myModule = module;
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final Sdk sdk = ModuleRootManager.getInstance(myModule).getSdk();
    assert sdk != null;

    GeneralCommandLine commandLine = getCommand(sdk);

    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }

  private GeneralCommandLine getCommand(Sdk sdk) throws ExecutionException {
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    String erl = FileUtil.toSystemDependentName(ErlangSdkType.getTopLevelExecutable(sdk.getHomePath()).getAbsolutePath());
    commandLine.setExePath(erl);
    commandLine.setWorkDirectory(myModule.getProject().getBasePath());
    commandLine.addParameters(ErlangConsoleUtil.getCodePath(myModule, useTestCodePath()));
    setUpCommandLineParameters(commandLine);
    final TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(myModule.getProject());
    setConsoleBuilder(consoleBuilder);
    return commandLine;
  }

  protected abstract void setUpCommandLineParameters(GeneralCommandLine commandLine) throws ExecutionException;

  protected abstract boolean useTestCodePath();
}
