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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.progress.util.ProgressIndicatorBase;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
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
    FileUtil.copyDir(new File("testData/mockSdk-R15B02"), myTempDir);
    // The SDK's escript stands for a rebar get-deps that does not finish (an unreachable host): it prints and sleeps.
    File escript = JpsErlangSdkType.getScriptInterpreterExecutable(myTempDir.getPath());
    assertTrue(escript.getParentFile().mkdirs());
    Files.writeString(escript.toPath(), "#!/bin/sh\necho " + STARTED + "\nexec sleep 600\n", StandardCharsets.UTF_8);
    assertTrue(escript.setExecutable(true));
    Sdk sdk = ErlangSdkType.createMockSdk(myTempDir.getPath(), ErlangSdkRelease.V_R15B02);
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable.getInstance().addJdk(sdk);
      ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
    });
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      // Kill what the fetch left running, so that a failure here does not leave a sleeping process behind.
      ProcessHandle.current().descendants().forEach(ProcessHandle::destroyForcibly);
      if (myTempDir != null) FileUtil.delete(myTempDir);
    }
    finally {
      super.tearDown();
    }
  }

  // #1306: Cancel in the "Fetching Dependencies" dialog cancels the indicator; the fetch must stop and kill rebar.
  public void testFetchDependenciesStopsOnCancel() throws Exception {
    VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(myTempDir);
    assertNotNull(projectRoot);
    Task.Modal task = createFetchDependenciesTask(getProject(), projectRoot, new File(myTempDir, "rebar").getPath());
    assertTrue("The fetch must be cancellable", task.isCancellable());

    ProgressIndicator indicator = new ProgressIndicatorBase();
    Future<?> fetch = ApplicationManager.getApplication().executeOnPooledThread(() -> task.run(indicator));

    long deadline = System.currentTimeMillis() + 60_000;
    while (!String.valueOf(indicator.getText2()).contains(STARTED)) {
      assertFalse("The fetch ended before rebar printed anything: " + indicator.getText2(), fetch.isDone());
      assertTrue("rebar did not start", System.currentTimeMillis() < deadline);
      Thread.sleep(50);
    }
    List<ProcessHandle> rebar = ProcessHandle.current().descendants().toList();
    assertFalse("rebar is not running", rebar.isEmpty());

    indicator.cancel(); // what the Cancel button of the modal progress dialog does

    try {
      fetch.get(10, TimeUnit.SECONDS);
    }
    catch (TimeoutException e) {
      fail("The fetch did not stop within 10 s after Cancel; rebar is still alive: " +
           rebar.stream().filter(ProcessHandle::isAlive).map(ProcessHandle::pid).toList());
    }
    catch (java.util.concurrent.ExecutionException | java.util.concurrent.CancellationException ignored) {
      // a fetch that stops by throwing ProcessCanceledException is fine
    }
    for (ProcessHandle process : rebar) {
      process.onExit().get(10, TimeUnit.SECONDS);
    }
  }

  // The task of RebarProjectRootStep.fetchDependencies, which validate() skips in unit test mode.
  private static Task.Modal createFetchDependenciesTask(Project project, VirtualFile projectRoot, String rebarPath)
    throws Exception {
    Class<?> taskClass = Class.forName(RebarProjectRootStep.class.getName() + "$1");
    assertTrue(Task.Modal.class.isAssignableFrom(taskClass));
    Constructor<?> constructor = taskClass.getDeclaredConstructor(Project.class, String.class, boolean.class,
                                                                  Project.class, VirtualFile.class, String.class);
    constructor.setAccessible(true);
    return (Task.Modal)constructor.newInstance(project, "Fetching Dependencies", true, project, projectRoot, rebarPath);
  }
}
