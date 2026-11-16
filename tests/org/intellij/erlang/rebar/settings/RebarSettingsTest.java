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

import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;

public class RebarSettingsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testRebar3CmdWrapperNextToEscript() throws IOException {
    File directory = FileUtil.createTempDirectory("rebar_bin", null);
    File escript = createFile(directory, "rebar3");
    RebarSettings settings = createSettings(createFile(directory, "rebar3.cmd"));
    assertEquals(escript.getPath(), settings.getRebarEscriptPath());
    assertTrue(settings.isRebar3());
  }

  public void testRebarWrappersNextToEscript() throws IOException {
    for (String extension : new String[]{".bat", ".BAT", ".exe", ".ps1"}) {
      File directory = FileUtil.createTempDirectory("rebar_bin", null);
      File escript = createFile(directory, "rebar");
      RebarSettings settings = createSettings(createFile(directory, "rebar" + extension));
      assertEquals(extension, escript.getPath(), settings.getRebarEscriptPath());
      assertFalse(extension, settings.isRebar3());
    }
  }

  public void testRebar3CmdWrapperWithoutEscript() throws IOException {
    File wrapper = createFile(FileUtil.createTempDirectory("rebar_bin", null), "rebar3.cmd");
    RebarSettings settings = createSettings(wrapper);
    assertEquals(wrapper.getPath(), settings.getRebarEscriptPath());
    assertFalse(settings.isRebar3());
  }

  public void testRebar3Escript() throws IOException {
    File escript = createFile(FileUtil.createTempDirectory("rebar_bin", null), "rebar3");
    RebarSettings settings = createSettings(escript);
    assertEquals(escript.getPath(), settings.getRebarEscriptPath());
    assertTrue(settings.isRebar3());
  }

  private static RebarSettings createSettings(File rebar) {
    RebarSettings settings = new RebarSettings();
    settings.setRebarPath(rebar.getPath());
    return settings;
  }

  private static File createFile(File directory, String name) throws IOException {
    File file = new File(directory, name);
    FileUtil.writeToFile(file, "");
    return file;
  }
}
