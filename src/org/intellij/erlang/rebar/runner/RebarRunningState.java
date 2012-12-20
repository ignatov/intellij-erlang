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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.HyperlinkInfo;
import com.intellij.execution.filters.RegexpFilter;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

final class RebarRunningState extends CommandLineState {
  private final RebarRunConfiguration myConfig;

  public RebarRunningState(@NotNull final ExecutionEnvironment env, @NotNull final RebarRunConfiguration config) {
    super(env);
    myConfig = config;
    TextConsoleBuilder builder = TextConsoleBuilderFactory.getInstance().createBuilder(myConfig.getProject());
    builder.addFilter(new RegexpFilter(config.getProject(), "$FILE_PATH$:$LINE$:\\.*") {
      @Nullable
      @Override
      protected HyperlinkInfo createOpenFileHyperlink(String fileName, int line, int column) {
        HyperlinkInfo res = super.createOpenFileHyperlink(fileName, line, column);
        if (res == null) {
          Project project = config.getProject();
          RebarSettings rebarSettings = RebarSettings.getInstance(project);
          File parentFile = new File(rebarSettings.getRebarPath()).getParentFile();
          String absolutePath = new File(parentFile, fileName).getAbsolutePath();
          res = super.createOpenFileHyperlink(absolutePath, line, column);
        }
        return res;
      }
    });
    setConsoleBuilder(builder);
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    final RebarSettings rebarSettings = RebarSettings.getInstance(myConfig.getProject());
    commandLine.setWorkDirectory(myConfig.getProject().getBasePath());
    commandLine.setExePath(rebarSettings.getRebarPath());
    commandLine.addParameters(myConfig.getCommand().split("\\s+"));
    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }
}
