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
  private static final long STEP_MS = 1;
  private static final long OUTPUT_LIMIT = 1024 * 1024;
  private static final int MAX_LINE_LENGTH = 64 * 1024;
  private static final long DISPOSE_WAIT_MS = 2000;

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
      if (running != null && running.isDisposed()) return new ExtProcessOutput("", ""); // the IDE or the JVM is exiting
      // A new directory, random name and owner-only: rebar3 evaluates rebar.config.script from its working directory, a crash
      // dump stays out of the IDE's directory (#1239), and it is not deleted under a running command (a BEAM hangs in getcwd()).
      workDirectory = Files.createTempDirectory(Path.of(System.getProperty("java.io.tmpdir")), "erlang-ext-process-").toFile();
      CommandLine commandLine = new CommandLine(withAbsoluteExecutable(command), workDirectory);
      process = commandLine.createProcess();
      if (running != null && !running.register(process, workDirectory)) return new ExtProcessOutput("", "");
      process.getOutputStream().close();
      // A local process writes to the files and its streams end at once; one started through Eel writes to its streams.
      AtomicLong pipedOutput = new AtomicLong();
      readers = Executors.newFixedThreadPool(2, ConcurrencyUtil.newNamedThreadFactory("ExtProcessUtil output reader", true, Thread.NORM_PRIORITY));
      Future<String> stdOut = readFirstLine(readers, process.getInputStream(), pipedOutput);
      Future<String> stdErr = readFirstLine(readers, process.getErrorStream(), pipedOutput);
      long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeout);
      // Steps of 1 ms, so that a command whose output passes the limit is stopped early: `cat /dev/zero` writes gigabytes per
      // second, and may start to write only after a shell has started it. An exit ends a step at once.
      while (!process.waitFor(Math.min(STEP_MS, TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime())), TimeUnit.MILLISECONDS)) {
        boolean tooMuchOutput = commandLine.myStdOut.length() + commandLine.myStdErr.length() + pipedOutput.get() > OUTPUT_LIMIT;
        if (tooMuchOutput) {
          // Killed at once and before reading: a command that writes without stopping could fill the disk meanwhile.
          destroyTree(process, true);
          return commandLine.firstLines(stdOut, stdErr);
        }
        if (System.nanoTime() >= deadline) return new ExtProcessOutput("", "");
      }
      return commandLine.firstLines(stdOut, stdErr);
    }
    catch (InterruptedException e) {
      interrupted = true; // set again only after the kill below, which is waited for
    }
    catch (CancellationException e) {
      throw e; // after the cleanup below
    }
    catch (ExecutionException | IOException | RuntimeException e) {
      LOG.debug("Cannot run " + String.join(" ", command), e);
    }
    finally {
      if (process != null) cleanUp(process, workDirectory, running);
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
            else if (first.length() < MAX_LINE_LENGTH) first.append(buffer[i]);
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

  // Every step runs even when an earlier one fails. A command whose tree may still run stays known to the service, which
  // tries again to kill it and to delete its directory on dispose.
  static void cleanUp(@NotNull Process process, @NotNull File workDirectory, @Nullable RunningCommands running) {
    runQuietly(() -> {
      if (destroyAndCleanUp(process, workDirectory) && running != null) running.myCommands.remove(process);
    });
    runQuietly(() -> closeQuietly(process.getInputStream()));
    runQuietly(() -> closeQuietly(process.getErrorStream()));
  }

  private static void runQuietly(@NotNull Runnable step) {
    try {
      step.run();
    }
    catch (RuntimeException e) {
      LOG.warn(e);
    }
  }

  // The working directory is deleted only when the command and its descendants are known to be gone.
  static boolean destroyAndCleanUp(@NotNull Process process, @NotNull File workDirectory) {
    if (destroyTree(process, false)) return FileUtil.delete(workDirectory);
    LOG.debug("Process " + process + " or a descendant still runs; its working directory " + workDirectory + " is kept");
    return false;
  }

  // SIGTERM to the tree, then SIGKILL after a grace period, or SIGKILL at once when forcibly; waits for the exit.
  private static boolean destroyTree(@NotNull Process process, boolean forcibly) {
    Set<ProcessHandle> tree = new LinkedHashSet<>();
    if (!forcibly) {
      tree.addAll(kill(process, false));
      if (waitForExit(process, tree, GRACE_PERIOD_MS)) return true;
    }
    tree.addAll(kill(process, true));
    tree.forEach(ProcessHandle::destroyForcibly);
    return waitForExit(process, tree, KILL_WAIT_MS);
  }

  // The process is killed before its descendants, so that it cannot start new ones in their place. The descendants are
  // collected while the process is alive: once it is gone they are no longer its descendants. A process without a
  // ProcessHandle, as one started through Eel on 262, is killed alone.
  @NotNull
  private static Set<ProcessHandle> kill(@NotNull Process process, boolean forcibly) {
    Set<ProcessHandle> tree = new LinkedHashSet<>();
    try {
      ProcessHandle handle = process.toHandle();
      tree.add(handle);
      tree.addAll(handle.descendants().toList());
      if (forcibly) handle.destroyForcibly();
      else handle.destroy();
      tree.addAll(handle.descendants().toList());
      tree.forEach(forcibly ? ProcessHandle::destroyForcibly : ProcessHandle::destroy);
    }
    catch (UnsupportedOperationException e) {
      if (forcibly) process.destroyForcibly();
      else process.destroy();
    }
    return tree;
  }

  private static boolean waitForExit(@NotNull Process process, @NotNull Set<ProcessHandle> tree, long timeoutMs) {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs);
    try {
      if (!process.waitFor(timeoutMs, TimeUnit.MILLISECONDS)) return false;
      for (ProcessHandle handle : tree) {
        handle.onExit().get(Math.max(0, deadline - System.nanoTime()), TimeUnit.NANOSECONDS);
      }
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      return false;
    }
    catch (java.util.concurrent.ExecutionException | TimeoutException ignored) {
    }
    return tree.stream().noneMatch(ProcessHandle::isAlive);
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

  // The first line, read from at most the first MAX_LINE_LENGTH characters of the file.
  @NotNull
  private static String readLine(@NotNull File file) {
    try (Reader reader = new InputStreamReader(new FileInputStream(file))) {
      char[] buffer = new char[MAX_LINE_LENGTH];
      int length = 0;
      for (int read; length < buffer.length && (read = reader.read(buffer, length, buffer.length - length)) >= 0; ) {
        length += read;
      }
      String text = new String(buffer, 0, length);
      int end = StringUtil.indexOfAny(text, "\r\n");
      return end < 0 ? text : text.substring(0, end);
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
    private final Object myLock = new Object();
    private boolean myDisposed;
    @VisibleForTesting final Thread myShutdownHook = new Thread(this::dispose, "ExtProcessUtil shutdown");

    RunningCommands() {
      Runtime.getRuntime().addShutdownHook(myShutdownHook);
    }

    @Nullable
    static RunningCommands getInstance() {
      var application = ApplicationManager.getApplication();
      if (application == null || application.isDisposed()) return null;
      try {
        return application.getService(RunningCommands.class);
      }
      catch (CancellationException e) {
        return null; // the application is disposed meanwhile
      }
    }

    boolean isDisposed() {
      synchronized (myLock) {
        return myDisposed;
      }
    }

    // A command is either refused here or seen by dispose().
    boolean register(@NotNull Process process, @NotNull File workDirectory) {
      synchronized (myLock) {
        if (myDisposed) return false;
        myCommands.put(process, workDirectory);
        return true;
      }
    }

    // The thread that runs a command may not get to its finally block, so the directories are deleted here too. All commands
    // are killed first; then the wait for all of them has one bound.
    @Override
    public void dispose() {
      Map<Process, File> commands;
      synchronized (myLock) {
        myDisposed = true;
        commands = new HashMap<>(myCommands);
      }
      Map<Process, Set<ProcessHandle>> trees = new HashMap<>();
      commands.keySet().forEach(process -> runQuietly(() -> trees.put(process, kill(process, true))));
      long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(DISPOSE_WAIT_MS);
      while (!commands.isEmpty()) {
        commands.entrySet().removeIf(command -> {
          try {
            Set<ProcessHandle> tree = trees.getOrDefault(command.getKey(), Set.of());
            boolean gone = !command.getKey().isAlive() && tree.stream().noneMatch(ProcessHandle::isAlive);
            if (gone) FileUtil.delete(command.getValue());
            return gone;
          }
          catch (RuntimeException e) {
            LOG.warn(e);
            return true; // given up; its directory stays
          }
        });
        if (commands.isEmpty() || System.nanoTime() > deadline) break;
        try {
          //noinspection BusyWait
          Thread.sleep(10);
        }
        catch (InterruptedException e) {
          Thread.currentThread().interrupt();
          break;
        }
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
