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
import com.intellij.util.ConcurrencyUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;

public final class ExtProcessUtil {
  private ExtProcessUtil() {
  }

  private static final Logger LOG = Logger.getInstance(ExtProcessUtil.class);
  private static final long GRACE_PERIOD_MS = 1000;
  private static final long KILL_WAIT_MS = 5000;
  private static final long MAX_STEP_MS = 50;
  private static final long OUTPUT_LIMIT = 1024 * 1024;

  // Returns only after the command and the descendants it still has are gone: on timeout they get SIGTERM and then
  // SIGKILL, because a BEAM stuck in a dirty NIF does not stop on SIGTERM.
  @NotNull
  public static ExtProcessOutput execAndGetFirstLine(long timeout, @NotNull String... command) {
    File workDirectory = null;
    Process process = null;
    boolean interrupted = false;
    RunningCommands running = null;
    ExecutorService readers = null;
    try {
      running = RunningCommands.getInstance();
      if (running != null && running.myDisposed) return new ExtProcessOutput("", ""); // the IDE or the JVM is exiting
      // A new directory, random name and owner-only: rebar3 evaluates rebar.config.script from its working directory, a crash
      // dump stays out of the IDE's directory (#1239), and it is not deleted under a running command (a BEAM hangs in getcwd()).
      workDirectory = Files.createTempDirectory(Path.of(System.getProperty("java.io.tmpdir")), "erlang-ext-process-").toFile();
      CommandLine commandLine = new CommandLine(withAbsoluteExecutable(command), workDirectory);
      process = commandLine.createProcess();
      if (running != null) running.myCommands.put(process, workDirectory);
      if (running != null && running.myDisposed) return new ExtProcessOutput("", "");
      process.getOutputStream().close();
      // A local process writes to the files and its streams end at once; one started through Eel writes to its streams.
      AtomicLong pipedOutput = new AtomicLong();
      readers = Executors.newFixedThreadPool(2, ConcurrencyUtil.newNamedThreadFactory("ExtProcessUtil output reader", true, Thread.NORM_PRIORITY));
      Future<String> stdOut = readFirstLine(readers, process.getInputStream(), pipedOutput);
      Future<String> stdErr = readFirstLine(readers, process.getErrorStream(), pipedOutput);
      long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeout);
      // Short steps, growing from 1 ms, so that a command whose output passes the limit is stopped early: `yes` writes about
      // a gigabyte per second. An exit ends a step at once.
      for (long step = 1; !process.waitFor(Math.min(step, TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime())), TimeUnit.MILLISECONDS);
           step = Math.min(2 * step, MAX_STEP_MS)) {
        boolean tooMuchOutput = commandLine.myStdOut.length() + commandLine.myStdErr.length() + pipedOutput.get() > OUTPUT_LIMIT;
        if (tooMuchOutput) return commandLine.firstLines(stdOut, stdErr);
        if (System.nanoTime() >= deadline) return new ExtProcessOutput("", "");
      }
      return commandLine.firstLines(stdOut, stdErr);
    }
    catch (InterruptedException e) {
      interrupted = true; // set again only after the kill below, which is waited for
    }
    catch (ExecutionException | IOException | CancellationException ignored) {
      // CancellationException: the application is disposed, getting the service fails.
    }
    finally {
      if (process != null) {
        destroyAndCleanUp(process.toHandle(), workDirectory);
        if (running != null) running.myCommands.remove(process);
        closeQuietly(process.getInputStream());
        closeQuietly(process.getErrorStream());
      }
      else if (workDirectory != null) FileUtil.delete(workDirectory);
      if (readers != null) readers.shutdown();
      if (interrupted) Thread.currentThread().interrupt();
    }
    return new ExtProcessOutput("", "");
  }

  // A local command writes to files in its directory, so that a process it detached cannot keep a pipe and a reader thread
  // busy after the call. createProcess() still decides how to start it: the Eel launcher, which starts commands in WSL or on
  // remote targets, ignores the redirects, so such a command keeps its pipes.
  private static final class CommandLine extends GeneralCommandLine {
    final File myStdOut;
    final File myStdErr;

    CommandLine(@NotNull String[] command, @NotNull File workDirectory) {
      super(command);
      withWorkDirectory(workDirectory);
      myStdOut = new File(workDirectory, "stdout");
      myStdErr = new File(workDirectory, "stderr");
    }

    @NotNull
    @Override
    protected Process createProcess(@NotNull ProcessBuilder builder) throws IOException {
      return super.createProcess(builder.redirectOutput(myStdOut).redirectError(myStdErr));
    }

    @NotNull
    ExtProcessOutput firstLines(@NotNull Future<String> stdOut, @NotNull Future<String> stdErr) throws InterruptedException {
      String out = readLine(myStdOut);
      String err = readLine(myStdErr);
      return new ExtProcessOutput(out.isEmpty() ? firstLine(stdOut) : out, err.isEmpty() ? firstLine(stdErr) : err);
    }
  }

  // A stream ends when the process exits, unless a process that the command detached still holds it.
  @NotNull
  private static String firstLine(@NotNull Future<String> line) throws InterruptedException {
    try {
      return line.get(GRACE_PERIOD_MS, TimeUnit.MILLISECONDS);
    }
    catch (java.util.concurrent.ExecutionException | TimeoutException e) {
      return "";
    }
  }

  // Reads a pipe to its end and counts what it reads; the first line is available as soon as it is read.
  @NotNull
  private static Future<String> readFirstLine(@NotNull ExecutorService executor, @NotNull InputStream stream, @NotNull AtomicLong size) {
    CompletableFuture<String> line = new CompletableFuture<>();
    executor.execute(() -> {
      StringBuilder first = new StringBuilder();
      try (Reader reader = new InputStreamReader(stream)) {
        char[] buffer = new char[8192];
        for (int read; (read = reader.read(buffer)) >= 0; ) {
          size.addAndGet(read);
          for (int i = 0; i < read && !line.isDone(); i++) {
            if (buffer[i] == '\n' || buffer[i] == '\r') line.complete(first.toString());
            else if (first.length() < OUTPUT_LIMIT) first.append(buffer[i]);
          }
        }
      }
      catch (IOException ignored) {
      }
      line.complete(first.toString());
    });
    return line;
  }

  private static void closeQuietly(@NotNull InputStream stream) {
    try {
      stream.close();
    }
    catch (IOException ignored) {
    }
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
    final Map<Process, File> myCommands = new ConcurrentHashMap<>();
    volatile boolean myDisposed;
    @VisibleForTesting final Thread myShutdownHook = new Thread(this::dispose, "ExtProcessUtil shutdown");

    RunningCommands() {
      Runtime.getRuntime().addShutdownHook(myShutdownHook);
    }

    @Nullable
    static RunningCommands getInstance() {
      var application = ApplicationManager.getApplication();
      return application == null || application.isDisposed() ? null : application.getService(RunningCommands.class);
    }

    // The thread that runs a command may not get to its finally block, so the directories are deleted here too.
    @Override
    public void dispose() {
      myDisposed = true;
      Map<Set<ProcessHandle>, File> trees = new HashMap<>();
      myCommands.forEach((process, directory) -> {
        Set<ProcessHandle> tree = new LinkedHashSet<>(process.descendants().toList());
        tree.add(process.toHandle());
        process.destroyForcibly();
        tree.forEach(ProcessHandle::destroyForcibly);
        trees.put(tree, directory);
      });
      trees.forEach((tree, directory) -> {
        if (waitForExit(tree, KILL_WAIT_MS)) FileUtil.delete(directory);
      });
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
