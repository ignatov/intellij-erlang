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

package org.intellij.erlang.rebar.settings;

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.concurrent.atomic.AtomicReference;

public class RebarConfigurationFormTest extends BasePlatformTestCase {
  // The version check runs in the temp directory; a rebar path relative to the IDE's directory must still work.
  public void testVersionOfRebarWithRelativePath() throws IOException {
    File directory = FileUtil.createTempDirectory(new File("build"), "relativeRebar", null, true);
    try {
      File rebar = new File(directory, SystemInfo.isWindows ? "rebar.cmd" : "rebar");
      Files.writeString(rebar.toPath(), SystemInfo.isWindows ? "@echo rebar 9.9.9\r\n" : "#!/bin/sh\necho rebar 9.9.9\n",
                        StandardCharsets.UTF_8);
      assertTrue(rebar.setExecutable(true));
      String relativePath = FileUtil.getRelativePath(new File("").getAbsoluteFile(), rebar);
      assertNotNull(relativePath);
      assertFalse(new File(relativePath).isAbsolute());

      AtomicReference<String> version = new AtomicReference<>();
      RebarConfigurationForm.validateRebarPath(relativePath, null, version::set);
      long deadline = System.currentTimeMillis() + 20000;
      while (version.get() == null && System.currentTimeMillis() < deadline) {
        PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        PlatformTestUtil.waitForAlarm(10);
      }
      assertEquals("rebar 9.9.9", version.get());
    }
    finally {
      FileUtil.delete(directory);
    }
  }

  // #698: when neither the rebar file nor the escript can be started (here: a rebar without the executable bit and an
  // Erlang SDK without bin/escript, as with an IDE whose PATH has no escript), the check must report why, not stay silent.
  public void testRebarAndEscriptCannotBeStarted() throws IOException {
    if (SystemInfo.isWindows) return;
    File directory = FileUtil.createTempDirectory("rebar698", null, true);
    Project project = getProject();
    Sdk previousSdk = ProjectRootManager.getInstance(project).getProjectSdk();
    try {
      File rebar = new File(directory, "rebar");
      Files.writeString(rebar.toPath(),
                        "#!/usr/bin/env escript\nmain(_) -> io:format(\"rebar 2.6.1 18 20151014_015616 No VCS info available.~n\").\n",
                        StandardCharsets.UTF_8);
      assertTrue(rebar.setExecutable(false, false));
      assertFalse(rebar.canExecute());

      String sdkHome = new File("testData/mockSdk-R15B02").getAbsolutePath();
      assertFalse(new File(sdkHome, "bin/escript").exists());
      Sdk sdk = ErlangSdkType.createMockSdk(sdkHome, ErlangSdkRelease.V_R15B02);
      WriteAction.runAndWait(() -> {
        ProjectJdkTable.getInstance().addJdk(sdk, getTestRootDisposable());
        ProjectRootManager.getInstance(project).setProjectSdk(sdk);
      });
      assertEquals(sdkHome, ErlangSdkType.getSdkPath(project));

      AtomicReference<String> message = new AtomicReference<>();
      RebarConfigurationForm.validateRebarPath(rebar.getPath(), project, message::set);
      long deadline = System.currentTimeMillis() + 10000;
      while (message.get() == null && System.currentTimeMillis() < deadline) {
        PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        PlatformTestUtil.waitForAlarm(10);
      }
      assertNotNull("The version check reported nothing although neither rebar nor escript could be started", message.get());
      assertFalse(message.get(), message.get().startsWith("rebar"));
    }
    finally {
      WriteAction.runAndWait(() -> ProjectRootManager.getInstance(project).setProjectSdk(previousSdk));
      FileUtil.delete(directory);
    }
  }
}
