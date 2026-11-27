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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class RebarProjectRootStepTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String STARTED = "fetching";
  private static final Pattern STARTED_PID = Pattern.compile(STARTED + " (\\d+)");

  private File myTempDir;
  private ProcessHandle myRebar;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myTempDir = FileUtil.createTempDirectory("rebarFetch", null);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      // Kill what the fetch left running, so that a failure here does not leave a blocked process behind.
      if (myRebar != null) Stream.concat(myRebar.descendants(), Stream.of(myRebar)).forEach(ProcessHandle::destroyForcibly);
      if (myTempDir != null) FileUtil.delete(myTempDir);
    }
    finally {
      super.tearDown();
    }
  }

  // #1306: Cancel in the "Fetching Dependencies" dialog cancels the indicator; the fetch must stop and kill rebar.
  public void testFetchDependenciesStopsOnCancel() throws Exception {
    ProgressIndicator indicator = new ProgressIndicatorBase();
    GeneralCommandLine rebar = hangingRebar();
    Future<?> fetch = ApplicationManager.getApplication().executeOnPooledThread(
      () -> RebarProjectRootStep.runGetDeps(indicator, rebar));

    long deadline = System.currentTimeMillis() + 60_000;
    Matcher started;
    while (!(started = STARTED_PID.matcher(String.valueOf(indicator.getText2()))).find()) {
      assertFalse("The fetch ended before rebar printed anything: " + indicator.getText2(), fetch.isDone());
      assertTrue("rebar did not start", System.currentTimeMillis() < deadline);
      Thread.sleep(50);
    }
    // Only the process that the fetch started and its descendants; the test JVM can have other children (conhost.exe).
    myRebar = ProcessHandle.of(Long.parseLong(started.group(1))).orElseThrow();
    assertTrue("rebar is not a child of the test JVM", ProcessHandle.current().descendants().anyMatch(myRebar::equals));
    List<ProcessHandle> processes = Stream.concat(Stream.of(myRebar), myRebar.descendants()).toList();

    indicator.cancel(); // what the Cancel button of the modal progress dialog does

    try {
      fetch.get(10, TimeUnit.SECONDS);
    }
    catch (TimeoutException e) {
      fail("The fetch did not stop within 10 s after Cancel; rebar is still alive: " +
           processes.stream().filter(ProcessHandle::isAlive).map(ProcessHandle::pid).toList());
    }
    catch (java.util.concurrent.ExecutionException | java.util.concurrent.CancellationException ignored) {
      // a fetch that stops by throwing ProcessCanceledException is fine
    }
    for (ProcessHandle process : processes) {
      try {
        process.onExit().get(10, TimeUnit.SECONDS);
      }
      catch (TimeoutException e) {
        fail("rebar process " + process.pid() + " (" + process.info().command().orElse("?") + ") is alive 10 s after Cancel");
      }
    }
  }

  // Stands for a rebar get-deps that does not finish (an unreachable host): the test JVM's java runs a program
  // that prints its pid and then blocks on its standard input, which the fetch never closes. It works on every OS.
  private GeneralCommandLine hangingRebar() throws Exception {
    Path source = myTempDir.toPath().resolve("HangingRebar.java");
    Files.writeString(source, "public class HangingRebar {\n" +
                              "  public static void main(String[] args) throws Exception {\n" +
                              "    System.out.println(\"" + STARTED + " \" + ProcessHandle.current().pid());\n" +
                              "    System.out.flush();\n" +
                              "    while (System.in.read() != -1) {}\n" +
                              "    Thread.sleep(600_000);\n" +
                              "  }\n" +
                              "}\n", StandardCharsets.UTF_8);
    String java = ProcessHandle.current().info().command().orElseThrow();
    return new GeneralCommandLine(java, source.toString()).withWorkDirectory(myTempDir);
  }
}
