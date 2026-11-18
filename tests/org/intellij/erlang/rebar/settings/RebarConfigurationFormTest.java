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

import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

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

  // A version check that ends after the path has changed again must not show its version next to the new path.
  public void testVersionOfReplacedPathIsNotShown() throws Exception {
    if (SystemInfo.isWindows) return;
    File directory = FileUtil.createTempDirectory("replacedRebar", null, true);
    File slow = new File(directory, "slow");
    Files.writeString(slow.toPath(), "#!/bin/sh\nsleep 1\necho rebar 1.0.0 slow\n", StandardCharsets.UTF_8);
    assertTrue(slow.setExecutable(true));
    File fast = new File(directory, "fast");
    Files.writeString(fast.toPath(), "#!/bin/sh\necho rebar 2.0.0 fast\n", StandardCharsets.UTF_8);
    assertTrue(fast.setExecutable(true));

    RebarConfigurationForm form = new RebarConfigurationForm();
    form.setPath(slow.getPath());
    form.setPath(fast.getPath());
    long deadline = System.currentTimeMillis() + 2500;
    while (System.currentTimeMillis() < deadline) {
      PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
      PlatformTestUtil.waitForAlarm(10);
    }
    assertEquals(fast.getPath(), form.getPath());
    assertEquals("rebar 2.0.0 fast", getVersionText(form));
  }

  private static String getVersionText(RebarConfigurationForm form) throws ReflectiveOperationException {
    Field field = RebarConfigurationForm.class.getDeclaredField("myRebarVersionText");
    field.setAccessible(true);
    return ((javax.swing.JTextField) field.get(form)).getText();
  }
}
