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

public class RebarProjectRootStepTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String STARTED = "fetching";

  private File myTempDir;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myTempDir = FileUtil.createTempDirectory("rebarFetch", null);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      // Kill what the fetch left running, so that a failure here does not leave a blocked process behind.
      ProcessHandle.current().descendants().forEach(ProcessHandle::destroyForcibly);
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
    while (!String.valueOf(indicator.getText2()).contains(STARTED)) {
      assertFalse("The fetch ended before rebar printed anything: " + indicator.getText2(), fetch.isDone());
      assertTrue("rebar did not start", System.currentTimeMillis() < deadline);
      Thread.sleep(50);
    }
    List<ProcessHandle> processes = ProcessHandle.current().descendants().toList();
    assertFalse("rebar is not running", processes.isEmpty());

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
      process.onExit().get(10, TimeUnit.SECONDS);
    }
  }

  // Stands for a rebar get-deps that does not finish (an unreachable host): the test JVM's java runs a program
  // that prints and then blocks on its standard input, which the fetch never closes. It works on every OS.
  private GeneralCommandLine hangingRebar() throws Exception {
    Path source = myTempDir.toPath().resolve("HangingRebar.java");
    Files.writeString(source, "public class HangingRebar {\n" +
                              "  public static void main(String[] args) throws Exception {\n" +
                              "    System.out.println(\"" + STARTED + "\");\n" +
                              "    System.out.flush();\n" +
                              "    while (System.in.read() != -1) {}\n" +
                              "    Thread.sleep(600_000);\n" +
                              "  }\n" +
                              "}\n", StandardCharsets.UTF_8);
    String java = ProcessHandle.current().info().command().orElseThrow();
    return new GeneralCommandLine(java, source.toString()).withWorkDirectory(myTempDir);
  }
}
