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
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

@RunWith(JUnit38AssumeSupportRunner.class)
public class ExtProcessUtilTest extends BasePlatformTestCase {
  // The rebar version check runs through execAndGetFirstLine; a rebar3 that loses its stdout
  // writes rebar3.crashdump into its working directory, so that must not be the IDE's one.
  // It is a fresh directory in the temp directory, which is not deleted while the command runs and is removed after it.
  public void testCommandRunsInPrivateTempDirectory() throws IOException {
    String[] printWorkingDirectory = SystemInfo.isWindows ? new String[]{"cmd", "/c", "cd"} : new String[]{"pwd"};
    String workingDirectory = ExtProcessUtil.execAndGetFirstLine(10000, printWorkingDirectory).getStdOut();
    assertFalse("no output from " + String.join(" ", printWorkingDirectory), workingDirectory.isEmpty());
    File directory = new File(workingDirectory).getCanonicalFile();
    assertEquals(new File(System.getProperty("java.io.tmpdir")).getCanonicalPath(), directory.getParent());
    assertFalse(directory.getPath(), directory.exists());
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
    assertTrue(command.setExecutable(true));

    ExtProcessUtil.execAndGetFirstLine(2000, command.getPath());

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
    assertTrue(command.setExecutable(true));

    long start = System.nanoTime();
    ExtProcessUtil.ExtProcessOutput output = ExtProcessUtil.execAndGetFirstLine(10000, command.getPath());

    assertEquals("first", output.getStdOut());
    assertEquals("error", output.getStdErr());
    assertTrue(TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start) < 10000);
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
    assertTrue(wrapper.setExecutable(true));

    ExtProcessUtil.execAndGetFirstLine(1000, wrapper.getPath());

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
