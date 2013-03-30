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
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Processor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;

public class ErlangConsoleCommandLineState extends CommandLineState {
  @NotNull private final ErlangConsoleRunConfiguration myConfig;

  public ErlangConsoleCommandLineState(@NotNull ErlangConsoleRunConfiguration config,
                                       @NotNull ExecutionEnvironment env) {
    super(env);
    myConfig = config;
    final TextConsoleBuilder consoleBuilder = new TextConsoleBuilderImpl(myConfig.getProject()) {
      @Override
      public ConsoleView getConsole() {
        final ErlangConsoleView consoleView = new ErlangConsoleView(myConfig.getProject());
        ErlangConsoleUtil.attachFilters(myConfig.getProject(), consoleView);
        return consoleView;
      }
    };
    setConsoleBuilder(consoleBuilder);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final Project project = myConfig.getProject();
    final Module module = myConfig.getConfigurationModule().getModule();
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setExePath(getErlPath(project, module));
    final String consoleArgs = myConfig.getConsoleArgs();
    if (!StringUtil.isEmpty(consoleArgs)) {
      commandLine.addParameters(consoleArgs);
    }
    commandLine.addParameters(getCodePath(project, module));
    commandLine.setWorkDirectory(getWorkingDirPath(project, myConfig.getWorkingDirPath()));
    final OSProcessHandler handler = new OSProcessHandler(
      commandLine.createProcess(), commandLine.getCommandLineString());
    ProcessTerminatedListener.attach(handler);
    return handler;
  }

  @NotNull
  private static String getWorkingDirPath(@NotNull Project project, @NotNull String workingDirPath) {
    if (workingDirPath.isEmpty()) {
      return project.getBasePath();
    }
    else {
      return workingDirPath;
    }
  }

  @NotNull
  private static String getErlPath(@NotNull Project project, @Nullable Module module) throws ExecutionException {
    final Sdk sdk;
    if (module != null) {
      sdk = ModuleRootManager.getInstance(module).getSdk();
    }
    else {
      sdk = ProjectRootManager.getInstance(project).getProjectSdk();
    }
    if (sdk == null) {
      throw new ExecutionException("Erlang SDK is not configured");
    }
    return sdk.getHomePath() + File.separator + "bin" + File.separator + "erl";
  }

  @NotNull
  private static List<String> getCodePath(@NotNull Project project, @Nullable Module module) {
    final Set<Module> codePathModules = new HashSet<Module>();
    if (module != null) {
      final ModuleRootManager moduleRootMgr = ModuleRootManager.getInstance(module);
      moduleRootMgr.orderEntries().recursively().forEachModule(new Processor<Module>() {
        @Override
        public boolean process(@NotNull Module dependencyModule) {
          codePathModules.add(dependencyModule);
          return true;
        }
      });
    }
    else {
      codePathModules.addAll(Arrays.asList(ModuleManager.getInstance(project).getModules()));
    }

    final List<String> codePath = new ArrayList<String>(codePathModules.size() * 2);
    for (Module codePathModule : codePathModules) {
      final ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(codePathModule);
      final CompilerModuleExtension compilerModuleExt =
        moduleRootManager.getModuleExtension(CompilerModuleExtension.class);
      final VirtualFile compilerOutput = compilerModuleExt.getCompilerOutputPath();
      if (compilerOutput != null) {
        codePath.add("-pa");
        codePath.add(compilerOutput.getCanonicalPath());
      }
    }

    return codePath;
  }
}
