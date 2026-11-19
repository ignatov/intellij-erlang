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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ConcurrencyUtil;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public final class ExtProcessUtil {
  private ExtProcessUtil() {
  }

  private static final long GRACE_PERIOD_MS = 1000;
  private static final long KILL_WAIT_MS = 5000;
  // Commands that are still running when the IDE or the test JVM exits, before their timeout, are killed then.
  private static final Set<Process> ourRunning = ConcurrentHashMap.newKeySet();

  static {
    Runtime.getRuntime().addShutdownHook(new Thread(() -> ourRunning.forEach(process -> {
      process.descendants().forEach(ProcessHandle::destroyForcibly);
      process.destroyForcibly();
    }), "ExtProcessUtil shutdown"));
  }

  // Returns only after the command and the descendants it still has are gone: on timeout they get SIGTERM and then
  // SIGKILL, because a BEAM stuck in a dirty NIF does not stop on SIGTERM.
  @NotNull
  public static ExtProcessOutput execAndGetFirstLine(long timeout, @NotNull String... command) {
    File workDirectory = null;
    Process process = null;
    ExecutorService readers = Executors.newFixedThreadPool(2, ConcurrencyUtil.newNamedThreadFactory("ExtProcessUtil output reader", true, Thread.NORM_PRIORITY));
    try {
      // A private empty directory: a crash dump of the command does not land in the IDE's directory (#1239), the start is
      // not slowed down by a large shared temp directory, and the directory is not deleted while the command runs, as the
      // temp directory of a test is at its end. With a deleted working directory under a large parent, the BEAM of rebar3
      // hangs in getcwd() while booting.
      workDirectory = FileUtil.createTempDirectory(new File(System.getProperty("java.io.tmpdir")), "erlang-ext-process", null, false);
      process = new GeneralCommandLine(withAbsoluteExecutable(command)).withWorkDirectory(workDirectory).createProcess();
      ourRunning.add(process);
      process.getOutputStream().close();
      // Both streams are read while the command runs, so a command that writes more than a pipe buffer is not blocked.
      Future<String> stdOut = readFirstLine(readers, process.getInputStream());
      Future<String> stdErr = readFirstLine(readers, process.getErrorStream());
      if (process.waitFor(timeout, TimeUnit.MILLISECONDS)) {
        return new ExtProcessOutput(firstLine(stdOut), firstLine(stdErr));
      }
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
    }
    catch (ExecutionException | IOException ignored) {
    }
    finally {
      if (process != null) {
        destroyProcessTree(process);
        ourRunning.remove(process);
      }
      readers.shutdown();
      if (workDirectory != null) FileUtil.delete(workDirectory);
    }
    return new ExtProcessOutput("", "");
  }

  // A stream ends when the process exits, unless a descendant of the process still holds it.
  @NotNull
  private static String firstLine(@NotNull Future<String> line) throws InterruptedException {
    try {
      return line.get(GRACE_PERIOD_MS, TimeUnit.MILLISECONDS);
    }
    catch (java.util.concurrent.ExecutionException | TimeoutException e) {
      return "";
    }
  }

  private static long remainingMs(long deadline) {
    return Math.max(0, TimeUnit.NANOSECONDS.toMillis(deadline - System.nanoTime()));
  }

  private static void destroyProcessTree(@NotNull Process process) {
    // The descendants are collected while the process is alive: once it is gone they are no longer its descendants.
    Set<ProcessHandle> tree = new LinkedHashSet<>(process.descendants().toList());
    tree.add(process.toHandle());
    tree.forEach(ProcessHandle::destroy);
    if (waitForExit(tree, GRACE_PERIOD_MS)) return;
    if (process.isAlive()) tree.addAll(process.descendants().toList());
    tree.forEach(ProcessHandle::destroyForcibly);
    waitForExit(tree, KILL_WAIT_MS);
  }

  private static boolean waitForExit(@NotNull Set<ProcessHandle> processes, long timeoutMs) {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMs);
    for (ProcessHandle handle : processes) {
      try {
        handle.onExit().get(remainingMs(deadline), TimeUnit.MILLISECONDS);
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

  // The first line is available as soon as it is read; the rest of the output is read and dropped until the end of the stream.
  @NotNull
  private static Future<String> readFirstLine(@NotNull ExecutorService executor, @NotNull InputStream stream) {
    CompletableFuture<String> firstLine = new CompletableFuture<>();
    executor.execute(() -> {
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
        firstLine.complete(StringUtil.notNullize(reader.readLine()));
        char[] buffer = new char[8192];
        //noinspection StatementWithEmptyBody
        while (reader.read(buffer) >= 0) {
        }
      }
      catch (IOException ignore) {
        firstLine.complete("");
      }
    });
    return firstLine;
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
