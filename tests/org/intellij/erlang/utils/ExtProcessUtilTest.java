/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.jetbrains.annotations.NotNull;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

// The scripts run as `/bin/sh <script>`: macOS can take seconds to start a freshly written executable on a loaded machine.
@RunWith(JUnit38AssumeSupportRunner.class)
public class ExtProcessUtilTest extends BasePlatformTestCase {
  // The rebar version check runs through execAndGetFirstLine; a rebar3 that loses its stdout
  // writes rebar3.crashdump into its working directory, so that must not be the IDE's one.
  // It is a private directory instead of the shared temp directory, because `rebar3 --version` evaluates rebar.config.script
  // from its working directory. The directory is new for each call and is removed after it.
  public void testCommandRunsInPrivateTempDirectory() throws IOException {
    String[] printWorkingDirectory = SystemInfo.isWindows ? new String[]{"cmd", "/c", "cd"} : new String[]{"pwd"};
    String workingDirectory = ExtProcessUtil.execAndGetFirstLine(10000, printWorkingDirectory).getStdOut();
    assertFalse("no output from " + String.join(" ", printWorkingDirectory), workingDirectory.isEmpty());
    File directory = new File(workingDirectory).getCanonicalFile();
    assertEquals(new File(System.getProperty("java.io.tmpdir")).getCanonicalPath(), directory.getParent());
    assertFalse(directory.getPath(), directory.exists());
    // Each call has a directory of its own, so a check that still runs never shares it with a later one.
    String nextWorkingDirectory = ExtProcessUtil.execAndGetFirstLine(10000, printWorkingDirectory).getStdOut();
    assertFalse(nextWorkingDirectory.isEmpty());
    assertFalse(directory.getPath(), directory.equals(new File(nextWorkingDirectory).getCanonicalFile()));
  }

  // The directory is created with owner-only permissions, so another local user cannot put a rebar.config.script into it.
  public void testPrivateTempDirectoryIsOwnerOnly() {
    Assume.assumeFalse("POSIX permissions", SystemInfo.isWindows);
    String permissions = ExtProcessUtil.execAndGetFirstLine(10000, "/bin/sh", "-c", "ls -ld \"$PWD\" | cut -c1-10").getStdOut();
    assertEquals("drwx------", permissions);
  }

  // A directory that already exists under a predictable name, which another user could have made, is never used.
  public void testExistingDirectoryWithPredictableNameIsNotUsed() throws IOException {
    String[] printWorkingDirectory = SystemInfo.isWindows ? new String[]{"cmd", "/c", "cd"} : new String[]{"pwd"};
    File existing = new File(System.getProperty("java.io.tmpdir"), "erlang-ext-process");
    boolean created = existing.mkdir();
    try {
      Files.writeString(new File(existing, "rebar.config.script").toPath(), "marker.\n", StandardCharsets.UTF_8);
      String workingDirectory = ExtProcessUtil.execAndGetFirstLine(10000, printWorkingDirectory).getStdOut();
      assertFalse(workingDirectory.isEmpty());
      assertFalse(existing.getCanonicalFile().equals(new File(workingDirectory).getCanonicalFile()));
    }
    finally {
      if (created) FileUtil.delete(existing);
      else FileUtil.delete(new File(existing, "rebar.config.script"));
    }
  }

  // A command that does not stop on SIGTERM, here a shell that ignores it and its child, which inherits that, must not
  // outlive the timeout: rebar3 whose BEAM hangs in getcwd() in a deleted working directory ignores SIGTERM as well.
  public void testTimeoutKillsCommandAndChildThatIgnoreSigterm() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("ignoresTerm", null, true);
    File pidFile = new File(directory, "pid");
    File childPidFile = new File(directory, "child");
    File command = new File(directory, "ignores-term.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\ntrap '' TERM\nsleep 60 &\necho $! > '" + childPidFile.getPath() + "'\n" +
                                        "echo $$ > '" + pidFile.getPath() + "'\nwait\nsleep 60\n", StandardCharsets.UTF_8);

    ExtProcessUtil.execAndGetFirstLine(5000, "/bin/sh", command.getPath());

    boolean commandAlive = killIfAlive(pidFile);
    boolean childAlive = killIfAlive(childPidFile);
    assertFalse("the command is still running after execAndGetFirstLine returned", commandAlive);
    assertFalse("the child of the command is still running after execAndGetFirstLine returned", childAlive);
  }

  // The output is read while the command runs: a command that writes more than a pipe buffer is not blocked until the timeout.
  public void testCommandWithLongOutputDoesNotHang() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("longOutput", null, true);
    File command = new File(directory, "long-output.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\necho first\nyes 0123456789012345678901234567890123456789 | head -n 5000\n" +
                                        "yes error | head -n 20000 >&2\n", StandardCharsets.UTF_8);

    long start = System.nanoTime();
    ExtProcessUtil.ExtProcessOutput output = ExtProcessUtil.execAndGetFirstLine(10000, "/bin/sh", command.getPath());

    assertEquals("first", output.getStdOut());
    assertEquals("error", output.getStdErr());
    assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 10000);
  }

  // A command that writes without stopping, such as `yes` given as the rebar path, is stopped once its output passes the
  // limit, long before the timeout, and the first line is still returned.
  public void testEndlessOutputIsCutOff() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("endless", null, true);
    File pidFile = new File(directory, "pid");
    File command = new File(directory, "endless.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\necho $$ > '" + pidFile.getPath() + "'\n" +
                                        "while :; do echo 0123456789012345678901234567890123456789; done\n", StandardCharsets.UTF_8);

    long start = System.nanoTime();
    ExtProcessUtil.ExtProcessOutput output = ExtProcessUtil.execAndGetFirstLine(20000, "/bin/sh", command.getPath());
    long elapsed = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);

    boolean alive = killIfAlive(pidFile);
    assertEquals("0123456789012345678901234567890123456789", output.getStdOut());
    assertTrue("the call took " + elapsed + " ms", elapsed < 10000);
    assertFalse("the command is still running after execAndGetFirstLine returned", alive);
  }

  // A process outside the tree of the command, here one that the command detached, can keep stdout open after the command
  // exits. The call must neither wait for it nor leave threads of its own behind.
  public void testDetachedProcessHoldingOutputLeavesNoThreads() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("detached", null, true);
    File pidFile = new File(directory, "pid");
    File command = new File(directory, "detaches.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\necho v1\n(sleep 60 & echo $! > '" + pidFile.getPath() + "')\nexit 0\n",
                      StandardCharsets.UTF_8);
    try {
      long start = System.nanoTime();
      assertEquals("v1", ExtProcessUtil.execAndGetFirstLine(10000, "/bin/sh", command.getPath()).getStdOut());
      assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 30000);
      long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
      while (!ourThreads().isEmpty() && System.nanoTime() < deadline) {
        //noinspection BusyWait
        Thread.sleep(50);
      }
      assertEquals(List.of(), ourThreads());
    }
    finally {
      killIfAlive(pidFile);
    }
  }

  // The timeout reaches a grandchild of the command, a child of its child, that ignores SIGTERM as well.
  public void testTimeoutKillsGrandchildOfCommand() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("grandchild", null, true);
    File childPidFile = new File(directory, "child");
    File grandchildPidFile = new File(directory, "grandchild");
    File command = new File(directory, "grandchild.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\ntrap '' TERM\n" +
                                        "sh -c 'sleep 60 & echo $! > \"" + grandchildPidFile.getPath() + "\"; wait' &\n" +
                                        "echo $! > '" + childPidFile.getPath() + "'\nwait\nsleep 60\n", StandardCharsets.UTF_8);

    ExtProcessUtil.execAndGetFirstLine(5000, "/bin/sh", command.getPath());

    boolean childAlive = killIfAlive(childPidFile);
    boolean grandchildAlive = killIfAlive(grandchildPidFile);
    assertFalse("the child of the command is still running after execAndGetFirstLine returned", childAlive);
    assertFalse("the grandchild of the command is still running after execAndGetFirstLine returned", grandchildAlive);
  }

  // A command that starts its child again when the child dies must not leave a new child behind: it is killed first.
  public void testTimeoutKillsCommandThatRestartsItsChild() throws Exception {
    Assume.assumeFalse("the command is a shell script", SystemInfo.isWindows);
    File directory = FileUtil.createTempDirectory("restarts", null, true);
    File pidsFile = new File(directory, "pids");
    File command = new File(directory, "restarts.sh");
    Files.writeString(command.toPath(), "#!/bin/sh\ntrap '' TERM\nwhile :; do sleep 60 & echo $! >> '" + pidsFile.getPath() + "'; wait $!; done\n",
                      StandardCharsets.UTF_8);

    ExtProcessUtil.execAndGetFirstLine(5000, "/bin/sh", command.getPath());

    List<ProcessHandle> alive = new ArrayList<>();
    for (String pid : Files.readAllLines(pidsFile.toPath())) {
      ProcessHandle.of(Long.parseLong(pid.trim())).filter(ProcessHandle::isAlive).ifPresent(alive::add);
    }
    alive.forEach(ProcessHandle::destroyForcibly);
    assertEquals("children still running after execAndGetFirstLine returned", List.of(), alive);
  }

  // When the command is not known to be gone after the kill, its working directory stays: a BEAM whose working
  // directory is deleted under a large temp directory hangs in getcwd().
  public void testWorkingDirectoryStaysWhileCommandIsAlive() throws Exception {
    File directory = FileUtil.createTempDirectory("kept", null, true);
    ExtProcessUtil.destroyAndCleanUp(new UnkillableProcess(), directory);
    assertTrue(directory.isDirectory());
  }

  // Commands that still run when the application is disposed (IDE exit, plugin unload) are killed and their directories
  // deleted, as the thread that runs them may not get to it. The JVM shutdown
  // hook, which covers a JVM that exits without disposing the application, is removed so that it does not keep the plugin.
  public void testDisposeKillsRunningCommandsAndRemovesShutdownHook() throws Exception {
    Assume.assumeFalse("the command is sleep", SystemInfo.isWindows);
    ExtProcessUtil.RunningCommands commands = new ExtProcessUtil.RunningCommands();
    File directory = FileUtil.createTempDirectory("disposed", null, false);
    Process process = new ProcessBuilder("sleep", "60").directory(directory).start();
    try {
      commands.myCommands.put(process, directory);
      commands.dispose();
      assertTrue("the command is still running after dispose", process.waitFor(5, TimeUnit.SECONDS));
      assertFalse("the working directory of the command is still there after dispose", directory.exists());
      assertFalse("the shutdown hook is still registered after dispose", Runtime.getRuntime().removeShutdownHook(commands.myShutdownHook));
    }
    finally {
      process.destroyForcibly();
    }
  }

  @NotNull
  private static List<String> ourThreads() {
    return Thread.getAllStackTraces().keySet().stream().filter(Thread::isAlive).map(Thread::getName)
      .filter(name -> name.startsWith("ExtProcessUtil")).toList();
  }

  private static final class UnkillableProcess implements ProcessHandle {
    @Override public long pid() { return Long.MAX_VALUE; }
    @Override public Optional<ProcessHandle> parent() { return Optional.empty(); }
    @Override public Stream<ProcessHandle> children() { return Stream.empty(); }
    @Override public Stream<ProcessHandle> descendants() { return Stream.empty(); }
    @Override public Info info() { return ProcessHandle.current().info(); }
    @Override public CompletableFuture<ProcessHandle> onExit() { return new CompletableFuture<>(); }
    @Override public boolean supportsNormalTermination() { return true; }
    @Override public boolean destroy() { return true; }
    @Override public boolean destroyForcibly() { return true; }
    @Override public boolean isAlive() { return true; }
    @Override public int compareTo(@NotNull ProcessHandle other) { return Long.compare(pid(), other.pid()); }
  }

  private static boolean killIfAlive(@NotNull File pidFile) throws IOException {
    assertTrue(pidFile.getName() + " was not written", pidFile.exists());
    Optional<ProcessHandle> process = ProcessHandle.of(Long.parseLong(Files.readString(pidFile.toPath()).trim()));
    if (process.isEmpty() || !process.get().isAlive()) return false;
    process.get().destroyForcibly();
    return true;
  }

  // A rebar behind a wrapper script runs as a child of the wrapper; the timeout must stop that child too.
  public void testTimeoutStopsChildrenOfCommand() throws Exception {
    if (SystemInfo.isWindows) return;
    File directory = FileUtil.createTempDirectory("wrapper", null, true);
    File pidFile = new File(directory, "pid");
    File wrapper = new File(directory, "wrapper.sh");
    Files.writeString(wrapper.toPath(), "#!/bin/sh\nsleep 60 &\necho $! > '" + pidFile.getPath() + "'\nwait\n",
                      StandardCharsets.UTF_8);

    ExtProcessUtil.execAndGetFirstLine(5000, "/bin/sh", wrapper.getPath());

    long childPid = Long.parseLong(Files.readString(pidFile.toPath()).trim());
    Optional<ProcessHandle> child = ProcessHandle.of(childPid);
    try {
      if (child.isPresent()) {
        child.get().onExit().get(10, TimeUnit.SECONDS);
      }
    }
    catch (TimeoutException e) {
      child.get().destroyForcibly();
      fail("the child of the command is still running after the timeout");
    }
  }

  // The Emacs version check passes the path from the settings unchanged; a path relative to the IDE's directory must still start.
  public void testRelativeExecutableRunsFromTempDirectory() throws IOException {
    File directory = FileUtil.createTempDirectory(new File("build"), "relativeExecutable", null, true);
    try {
      File executable = new File(directory, SystemInfo.isWindows ? "emacs.cmd" : "emacs");
      Files.writeString(executable.toPath(), SystemInfo.isWindows ? "@echo GNU Emacs 99.9\r\n" : "#!/bin/sh\necho GNU Emacs 99.9\n",
                        StandardCharsets.UTF_8);
      assertTrue(executable.setExecutable(true));
      String relativePath = FileUtil.getRelativePath(new File("").getAbsoluteFile(), executable);
      assertNotNull(relativePath);
      assertFalse(new File(relativePath).isAbsolute());

      assertEquals("GNU Emacs 99.9", ExtProcessUtil.execAndGetFirstLine(10000, relativePath, "--version").getStdOut());
    }
    finally {
      FileUtil.delete(directory);
    }
  }
}
