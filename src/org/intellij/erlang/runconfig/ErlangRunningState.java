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
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.console.ErlangConsoleUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author savenko
 */
public abstract class ErlangRunningState extends CommandLineState {
  private final Module myModule;

  public ErlangRunningState(ExecutionEnvironment env, Module module) {
    super(env);
    myModule = module;
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    Sdk sdk = ModuleRootManager.getInstance(myModule).getSdk();
    assert sdk != null;
    GeneralCommandLine commandLine = getCommand(sdk);
    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }

  private GeneralCommandLine getCommand(@NotNull Sdk sdk) throws ExecutionException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    String homePath = sdk.getHomePath();
    assert homePath != null;
    String erl = FileUtil.toSystemDependentName(ErlangSdkType.getTopLevelExecutable(homePath).getAbsolutePath());
    commandLine.setExePath(erl);
    commandLine.setWorkDirectory(myModule.getProject().getBasePath());
    commandLine.addParameters(getCodePath());
    setEntryPoint(commandLine);
    setStopErlang(commandLine);
    setNoShellMode(commandLine);
    setErlangFlags(commandLine);
    TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(myModule.getProject());
    setConsoleBuilder(consoleBuilder);
    return commandLine;
  }

  public List<String> getCodePath() throws ExecutionException {
    return ErlangConsoleUtil.getCodePath(myModule, useTestCodePath());
  }

  public Module getModule() {
    return myModule;
  }

  protected abstract boolean useTestCodePath();

  protected abstract boolean isNoShellMode();

  protected abstract boolean isStopErlang();

  protected List<String> getErlFlags() {
    return ContainerUtil.emptyList();
  }

  public abstract ErlangEntryPoint getEntryPoint() throws ExecutionException;

  public ErlangEntryPoint getDebugEntryPoint() throws ExecutionException {
    return getEntryPoint();
  }

  private void setStopErlang(GeneralCommandLine commandLine) {
    if (isStopErlang()) {
      commandLine.addParameters("-s", "init", "stop");
    }
  }

  private void setNoShellMode(GeneralCommandLine commandLine) {
    if (isNoShellMode()) commandLine.addParameters("-noshell");
  }

  private void setEntryPoint(GeneralCommandLine commandLine) throws ExecutionException {
    ErlangEntryPoint entryPoint = getEntryPoint();
    commandLine.addParameters("-eval",
      entryPoint.getModuleName() + ":" + entryPoint.getFunctionName() +
        "(" + StringUtil.join(entryPoint.getArgsList(), ", ") + ").");
  }

  private void setErlangFlags(GeneralCommandLine commandLine) {
    commandLine.addParameters(getErlFlags());
  }

  @NotNull
  public abstract ConsoleView createConsoleView(Executor executor);

  public static class ErlangEntryPoint {
    private final String myModuleName;
    private final String myFunctionName;
    private final List<String> myArgsList;

    public ErlangEntryPoint(String moduleName, String functionName, List<String> args) {
      myModuleName = moduleName;
      myFunctionName = functionName;
      myArgsList = args;
    }

    public String getModuleName() {
      return myModuleName;
    }

    public String getFunctionName() {
      return myFunctionName;
    }

    public List<String> getArgsList() {
      return myArgsList;
    }
  }
}
