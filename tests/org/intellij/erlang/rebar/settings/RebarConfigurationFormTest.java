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

import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

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
}
