/*
 * Copyright 2012-2025 Sergey Ignatov
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

package org.intellij.erlang.jps.builder;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.incremental.ProjectBuildException;

import java.io.*;
import java.util.List;
import java.util.Map;

public final class ProcessRunner {
  private ProcessRunner() {
  }

  public static int runProcess(@NotNull List<String> command,
                               @Nullable File workingDirectory,
                               @Nullable Map<String, String> environment,
                               @NotNull BuilderProcessAdapter adapter) throws ProjectBuildException {
    ProcessBuilder processBuilder = new ProcessBuilder(command);
    if (workingDirectory != null) {
      processBuilder.directory(workingDirectory);
    }
    if (environment != null) {
      processBuilder.environment().putAll(environment);
    }
    processBuilder.redirectErrorStream(true);

    Process process;
    try {
      process = processBuilder.start();
    }
    catch (IOException e) {
      throw new ProjectBuildException("Failed to start process: " + String.join(" ", command), e);
    }

    try (BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
      String line;
      while ((line = reader.readLine()) != null) {
        adapter.onTextAvailable(line);
      }
    }
    catch (IOException e) {
      throw new ProjectBuildException("Failed to read process output", e);
    }

    try {
      int exitCode = process.waitFor();
      adapter.processTerminated();
      return exitCode;
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new ProjectBuildException("Process was interrupted", e);
    }
  }
}
