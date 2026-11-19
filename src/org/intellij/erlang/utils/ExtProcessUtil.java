/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.utils;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public final class ExtProcessUtil {
  private ExtProcessUtil() {
  }

  private static final Logger LOG = Logger.getInstance(ExtProcessUtil.class);
  private static final long GRACE_PERIOD_MS = 1000;
  private static final long KILL_WAIT_MS = 5000;

  // Returns only after the command and the descendants it still has are gone: on timeout they get SIGTERM and then
  // SIGKILL, because a BEAM stuck in a dirty NIF does not stop on SIGTERM.
  @NotNull
  public static ExtProcessOutput execAndGetFirstLine(long timeout, @NotNull String... command) {
    File workDirectory = null;
    Process process = null;
    boolean interrupted = false;
    RunningCommands running = RunningCommands.getInstance();
    try {
      // A private directory: a crash dump does not land in the IDE's directory (#1239), a large shared temp directory does
      // not slow the start, and the directory is not deleted under a running command (then a BEAM hangs in getcwd()).
      workDirectory = Files.createTempDirectory(Path.of(System.getProperty("java.io.tmpdir")), "erlang-ext-process-").toFile();
      // Output to files, not pipes: a command that writes a lot is not blocked, and a process that the command detached
      // cannot keep a pipe and a reader thread busy after the call.
      File stdOut = new File(workDirectory, "stdout");
      File stdErr = new File(workDirectory, "stderr");
      ProcessBuilder builder = new GeneralCommandLine(withAbsoluteExecutable(command)).withWorkDirectory(workDirectory).toProcessBuilder();
      process = builder.redirectOutput(stdOut).redirectError(stdErr).start();
      if (running != null) running.myProcesses.add(process);
      process.getOutputStream().close();
      if (process.waitFor(timeout, TimeUnit.MILLISECONDS)) {
        return new ExtProcessOutput(readLine(stdOut), readLine(stdErr));
      }
    }
    catch (InterruptedException e) {
      interrupted = true; // set again only after the kill below, which is waited for
    }
    catch (ExecutionException | IOException ignored) {
    }
    finally {
      if (process != null) {
        destroyAndCleanUp(process.toHandle(), workDirectory);
        if (running != null) running.myProcesses.remove(process);
      }
      else if (workDirectory != null) FileUtil.delete(workDirectory);
      if (interrupted) Thread.currentThread().interrupt();
    }
    return new ExtProcessOutput("", "");
  }

  // The working directory is deleted only when the command and its descendants are known to be gone.
  static void destroyAndCleanUp(@NotNull ProcessHandle process, @NotNull File workDirectory) {
    if (destroyProcessTree(process)) FileUtil.delete(workDirectory);
    else LOG.debug("Process " + process.pid() + " or a descendant still runs; its working directory " + workDirectory + " is kept");
  }

  // The process is killed before its descendants, so that it cannot start new ones in their place. The descendants are
  // collected while the process is alive: once it is gone they are no longer its descendants.
  private static boolean destroyProcessTree(@NotNull ProcessHandle process) {
    Set<ProcessHandle> tree = new LinkedHashSet<>();
    tree.add(process);
    tree.addAll(process.descendants().toList());
    process.destroy();
    tree.addAll(process.descendants().toList());
    tree.forEach(ProcessHandle::destroy);
    if (waitForExit(tree, GRACE_PERIOD_MS)) return true;
    tree.addAll(process.descendants().toList());
    process.destroyForcibly();
    tree.addAll(process.descendants().toList());
    tree.forEach(ProcessHandle::destroyForcibly);
    return waitForExit(tree, KILL_WAIT_MS);
  }

  private static boolean waitForExit(@NotNull Set<ProcessHandle> processes, long timeoutMs) {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs);
    for (ProcessHandle handle : processes) {
      try {
        handle.onExit().get(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        return false;
      }
      catch (java.util.concurrent.ExecutionException | TimeoutException ignored) {
      }
    }
    return processes.stream().noneMatch(ProcessHandle::isAlive);
  }

  // The command runs in the temp directory, so an executable path relative to the IDE's directory is made absolute.
  // A bare name without a separator is still looked up on PATH.
  @NotNull
  private static String[] withAbsoluteExecutable(@NotNull String[] command) {
    if (command.length == 0) return command;
    String executable = command[0];
    boolean hasSeparator = executable.indexOf('/') >= 0 || executable.indexOf(File.separatorChar) >= 0;
    if (!hasSeparator || new File(executable).isAbsolute()) return command;
    String[] result = command.clone();
    result[0] = new File(executable).getAbsolutePath();
    return result;
  }

  @NotNull
  private static String readLine(@NotNull File file) {
    try (BufferedReader errReader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))) {
      return StringUtil.notNullize(errReader.readLine());
    }
    catch (IOException ignore) {
      return "";
    }
  }

  // Kills the commands still running on IDE exit or plugin unload, and through a JVM shutdown hook on an exit without that,
  // as in a test JVM. Dispose removes the hook, so it does not keep the plugin's class loader after an unload.
  @Service(Service.Level.APP)
  static final class RunningCommands implements Disposable {
    final Set<Process> myProcesses = ConcurrentHashMap.newKeySet();
    @VisibleForTesting final Thread myShutdownHook = new Thread(this::dispose, "ExtProcessUtil shutdown");

    RunningCommands() {
      Runtime.getRuntime().addShutdownHook(myShutdownHook);
    }

    @Nullable
    static RunningCommands getInstance() {
      var application = ApplicationManager.getApplication();
      return application == null || application.isDisposed() ? null : application.getService(RunningCommands.class);
    }

    @Override
    public void dispose() {
      for (Process process : myProcesses) {
        var descendants = process.descendants().toList();
        process.destroyForcibly();
        descendants.forEach(ProcessHandle::destroyForcibly);
      }
      try {
        Runtime.getRuntime().removeShutdownHook(myShutdownHook);
      }
      catch (IllegalStateException ignored) {
        // The JVM is exiting: this is the hook.
      }
    }
  }

  public static class ExtProcessOutput {
    private final String myStdOut;
    private final String myStdErr;

    public ExtProcessOutput(@NotNull String stdOut, @NotNull String stdErr) {
      myStdOut = stdOut;
      myStdErr = stdErr;
    }

    @NotNull
    public String getStdOut() {
      return myStdOut;
    }

    @NotNull
    public String getStdErr() {
      return myStdErr;
    }
  }
}
