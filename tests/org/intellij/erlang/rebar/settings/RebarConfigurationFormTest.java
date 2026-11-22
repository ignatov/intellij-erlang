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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.sdk.ErlangSdkForSmallIdes;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;

import javax.swing.*;
import javax.swing.text.Document;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
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

  public void testVersionOfLocalInstallScriptUsesSdkPath() throws IOException {
    if (SystemInfo.isWindows) return;

    File sdkHome = FileUtil.createTempDirectory("rebarSdk", null, true);
    ProjectJdkTable sdkTable = ProjectJdkTable.getInstance();
    Sdk sdk = null;
    try {
      assertTrue(new File(sdkHome, "lib").mkdir());
      File sdkBin = new File(sdkHome, "bin");
      assertTrue(sdkBin.mkdir());
      File sdkErlExecutable = new File(sdkBin, "erl");
      Files.writeString(sdkErlExecutable.toPath(), "#!/bin/sh\nexit 0\n", StandardCharsets.UTF_8);
      assertTrue(sdkErlExecutable.setExecutable(true));
      Sdk testSdk = ErlangSdkType.createMockSdk(sdkHome.getPath(), ErlangSdkRelease.V_R15B02);
      sdk = testSdk;

      if (ErlangSystemUtil.isSmallIde()) {
        ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), sdkHome.getPath());
      }
      else {
        ApplicationManager.getApplication().runWriteAction(() -> {
          sdkTable.addJdk(testSdk);
          ProjectRootManager.getInstance(getProject()).setProjectSdk(testSdk);
        });
      }

      File directory = FileUtil.createTempDirectory("rebarLocalInstall", null, true);
      try {
        File rebar = new File(directory, "rebar3");
        Files.writeString(rebar.toPath(), "#!/bin/sh\necho \"rebar $(command -v erl)\"\n", StandardCharsets.UTF_8);
        assertTrue(rebar.setExecutable(true));

        AtomicReference<String> version = new AtomicReference<>();
        RebarConfigurationForm.validateRebarPath(rebar.getPath(), getProject(), version::set);
        long deadline = System.currentTimeMillis() + 20000;
        while (version.get() == null && System.currentTimeMillis() < deadline) {
          PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
          PlatformTestUtil.waitForAlarm(10);
        }

        String sdkPath = ErlangSdkType.getSdkPath(getProject());
        assertNotNull(sdkPath);
        String sdkErl = JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkPath).getAbsolutePath();
        assertEquals("rebar " + sdkErl, version.get());
      }
      finally {
        FileUtil.delete(directory);
      }
    }
    finally {
      if (ErlangSystemUtil.isSmallIde()) {
        ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), "");
      }
      else if (sdk != null) {
        Sdk sdkToRemove = sdk;
        ApplicationManager.getApplication().runWriteAction(() -> {
          ProjectRootManager.getInstance(getProject()).setProjectSdk(null);
          sdkTable.removeJdk(sdkToRemove);
        });
      }
      FileUtil.delete(sdkHome);
    }
  }

  // A rebar that needs more than 3 seconds to print its version, as a cold start on a busy Windows machine can,
  // must still be recognized.
  public void testVersionOfSlowRebar() throws IOException {
    File directory = FileUtil.createTempDirectory("slowRebar", null, true);
    try {
      File rebar = new File(directory, SystemInfo.isWindows ? "rebar.cmd" : "rebar");
      Files.writeString(rebar.toPath(), SystemInfo.isWindows ? "@ping -n 5 127.0.0.1 >nul\r\n@echo rebar 9.9.9\r\n"
                                                            : "#!/bin/sh\nsleep 4\necho rebar 9.9.9\n", StandardCharsets.UTF_8);
      assertTrue(rebar.setExecutable(true));

      AtomicReference<String> version = new AtomicReference<>();
      RebarConfigurationForm.validateRebarPath(rebar.getPath(), null, version::set);
      long deadline = System.currentTimeMillis() + 30000;
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

  // #1325: when neither probe of the new path prints anything, the version of the previous path must not stay,
  // or the new path counts as a valid rebar. The new path is typed as one character after the old one, so the
  // document gets one insert and no other path is checked in between.
  public void testVersionIsClearedWhenCheckGivesNoOutput() throws Exception {
    File directory = FileUtil.createTempDirectory("rebarVersionCleared", null, true);
    try {
      File rebar = new File(directory, SystemInfo.isWindows ? "rebar.cmd" : "rebar");
      Files.writeString(rebar.toPath(), SystemInfo.isWindows ? "@echo rebar 9.9.9\r\n" : "#!/bin/sh\necho rebar 9.9.9\n",
                        StandardCharsets.UTF_8);
      assertTrue(rebar.setExecutable(true));
      // Not executable, so the direct run cannot start; under escript it ends at once without output.
      File silent = new File(directory, rebar.getName() + "x");
      Files.writeString(silent.toPath(), "#!/usr/bin/env escript\nmain(_) -> ok.\n", StandardCharsets.UTF_8);

      RebarConfigurationForm form = new RebarConfigurationForm();
      long deadline = System.currentTimeMillis() + 20000;
      while (!(form.isPathValid() && form.getPath().equals(rebar.getPath())) && System.currentTimeMillis() < deadline) {
        form.setPath(rebar.getPath());
        PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        PlatformTestUtil.waitForAlarm(10);
      }
      assertEquals(rebar.getPath(), form.getPath());
      assertTrue(form.isPathValid());

      Field selectorField = RebarConfigurationForm.class.getDeclaredField("myRebarPathSelector");
      selectorField.setAccessible(true);
      Document document = ((TextFieldWithBrowseButton)selectorField.get(form)).getTextField().getDocument();
      document.insertString(document.getLength(), "x", null);
      assertEquals(silent.getPath(), form.getPath());

      long checkEnd = System.currentTimeMillis() + 10000;
      while (System.currentTimeMillis() < checkEnd) {
        PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        PlatformTestUtil.waitForAlarm(10);
      }
      assertEquals(silent.getPath(), form.getPath());
      assertFalse("the version of " + rebar.getPath() + " is kept for " + silent.getPath(), form.isPathValid());
    }
    finally {
      FileUtil.delete(directory);
    }
  }

  // #1325: the check of an earlier path that ends after the check of the current path must not show its version.
  public void testVersionOfEarlierPathDoesNotOverwriteCurrent() throws Exception {
    File directory = FileUtil.createTempDirectory("rebarVersionOrder", null, true);
    try {
      String extension = SystemInfo.isWindows ? ".cmd" : "";
      File slow = new File(directory, "slow" + extension);
      Files.writeString(slow.toPath(), SystemInfo.isWindows ? "@ping -n 2 127.0.0.1 >nul\r\n@echo rebar 1.0.0\r\n"
                                                           : "#!/bin/sh\nsleep 1\necho rebar 1.0.0\n", StandardCharsets.UTF_8);
      assertTrue(slow.setExecutable(true));
      File fast = new File(directory, "fast" + extension);
      Files.writeString(fast.toPath(), SystemInfo.isWindows ? "@echo rebar 2.0.0\r\n" : "#!/bin/sh\necho rebar 2.0.0\n",
                        StandardCharsets.UTF_8);
      assertTrue(fast.setExecutable(true));

      RebarConfigurationForm form = new RebarConfigurationForm();
      // Let the form's own lookup of the default rebar path end before the paths are set.
      dispatchEventsFor(1000);
      form.setPath(slow.getPath());
      form.setPath(fast.getPath());
      dispatchEventsFor(4000);

      Field versionField = RebarConfigurationForm.class.getDeclaredField("myRebarVersionText");
      versionField.setAccessible(true);
      assertEquals(fast.getPath(), form.getPath());
      assertEquals("rebar 2.0.0", ((JTextField)versionField.get(form)).getText());
    }
    finally {
      FileUtil.delete(directory);
    }
  }

  // #1416: an empty path is not a rebar; since JDK 25 it is the current directory, which the check used to run.
  public void testEmptyPathStartsNoProcess() {
    assertEquals("N/A", checkedVersion(""));
  }

  public void testBlankPathStartsNoProcess() {
    assertEquals("N/A", checkedVersion("  "));
  }

  // #1416: a directory is not run, directly or with escript.
  public void testDirectoryStartsNoProcess() throws IOException {
    File directory = FileUtil.createTempDirectory("rebarDirectory", null, true);
    try {
      assertEquals("Path " + directory.getPath() + " is a directory, not a file", checkedVersion(directory.getPath()));
    }
    finally {
      FileUtil.delete(directory);
    }
  }

  private static String checkedVersion(String rebarPath) {
    AtomicReference<String> version = new AtomicReference<>();
    RebarConfigurationForm.validateRebarPath(rebarPath, null, version::set);
    long deadline = System.currentTimeMillis() + 20000;
    while (version.get() == null && System.currentTimeMillis() < deadline) {
      PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
      PlatformTestUtil.waitForAlarm(10);
    }
    return version.get();
  }

  private static void dispatchEventsFor(long millis) {
    long end = System.currentTimeMillis() + millis;
    while (System.currentTimeMillis() < end) {
      PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
      PlatformTestUtil.waitForAlarm(10);
    }
  }
}
