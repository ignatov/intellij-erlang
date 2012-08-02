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

package org.intellij.erlang.sdk;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.*;

/**
 * @author ignatov
 */
public class ErlangSystemUtil {
  public static final int STANDARD_TIMEOUT = 10 * 1000;

  private ErlangSystemUtil() {
  }

  @NotNull
  public static ProcessOutput getProcessOutput(@NotNull final String workDir,
                                               @NotNull final String exePath,
                                               @NotNull final String... arguments) throws ExecutionException {
    return getProcessOutput(STANDARD_TIMEOUT, workDir, exePath, arguments);
  }

  @NotNull
  public static ProcessOutput getProcessOutput(final int timeout,
                                               @NotNull final String workDir,
                                               @NotNull final String exePath,
                                               @NotNull final String... arguments) throws ExecutionException {
    if (!new File(workDir).isDirectory() || !new File(exePath).canExecute()) {
      return new ProcessOutput();
    }

    final GeneralCommandLine cmd = new GeneralCommandLine();
    cmd.setWorkDirectory(workDir);
    cmd.setExePath(exePath);
    cmd.addParameters(arguments);

    return execute(cmd, timeout);
  }

  @NotNull
  public static ProcessOutput execute(@NotNull final GeneralCommandLine cmd) throws ExecutionException {
    return execute(cmd, STANDARD_TIMEOUT);
  }

  @NotNull
  public static ProcessOutput execute(@NotNull final GeneralCommandLine cmd, final int timeout) throws ExecutionException {
    final CapturingProcessHandler processHandler = new CapturingProcessHandler(cmd.createProcess());
    return timeout < 0 ? processHandler.runProcess() : processHandler.runProcess(timeout);
  }

  public static void addStdPaths(@NotNull final GeneralCommandLine cmd, @NotNull final Sdk sdk) {
    final List<VirtualFile> files = new ArrayList<VirtualFile>();
    files.addAll(Arrays.asList(sdk.getRootProvider().getFiles(OrderRootType.SOURCES)));
    files.addAll(Arrays.asList(sdk.getRootProvider().getFiles(OrderRootType.CLASSES)));
    final Set<String> paths = new HashSet<String>();
    for (final VirtualFile file : files) {
      paths.add(FileUtil.toSystemDependentName(file.getPath()));
    }
    for (final String path : paths) {
      cmd.addParameter("-I");
      cmd.addParameter(path);
    }
  }
}
