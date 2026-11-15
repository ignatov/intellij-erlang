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

package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;

public class RebarEunitRunningStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String REPORT_OPTION = "{report,{" + ErlangEunitReporterModule.MODULE_NAME + ", []}}";
  private static final String NO_TTY_OPTION = "{no_tty, true}";
  private static final String MODIFIED_CONFIG =
    "{erl_opts, [debug_info]}.\n{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ",verbose]}.\n";

  public void testWriteModifiedConfigWithCrlfLineSeparators() throws Exception {
    String config = doWriteModifiedConfig("{erl_opts, [debug_info]}.\r\n{eunit_opts, [verbose]}.\r\n");
    assertEquals(MODIFIED_CONFIG, config);
  }

  public void testWriteModifiedConfigWithCrlfLineSeparatorsAndNoEunitOpts() throws Exception {
    String config = doWriteModifiedConfig("{erl_opts, [debug_info]}.\r\n{deps, []}.\r\n");
    assertFalse(config.contains("\r"));
    assertTrue(config, config.startsWith("{erl_opts, [debug_info]}.\n{deps, []}.\n"));
    assertTrue(config, config.contains(REPORT_OPTION));
    assertTrue(config, config.contains(NO_TTY_OPTION));
  }

  public void testWriteModifiedConfigWithLfLineSeparators() throws Exception {
    String config = doWriteModifiedConfig("{erl_opts, [debug_info]}.\n{eunit_opts, [verbose]}.\n");
    assertEquals(MODIFIED_CONFIG, config);
  }

  public void testWriteModifiedConfigWithoutOldConfig() throws Exception {
    String config = doWriteModifiedConfig(null);
    assertTrue(config, config.contains(REPORT_OPTION));
    assertTrue(config, config.contains(NO_TTY_OPTION));
  }

  private String doWriteModifiedConfig(String oldConfigText) throws IOException {
    // Both directories are inside the temporary directory of the test, which the test framework deletes in tearDown.
    // They must not be deleted here: the platform finishes the write of the new config (its timestamp) later, and
    // from 2026.3 on tearDown fails with NoSuchFileException when the file is gone by then.
    File projectDirectory = FileUtil.createTempDirectory("rebar_project", null);
    File outputDirectory = FileUtil.createTempDirectory("rebar_eunit", null);
    File oldConfig = new File(projectDirectory, "rebar.config");
    if (oldConfigText != null) {
      FileUtil.writeToFile(oldConfig, oldConfigText);
    }
    File newConfig = new File(outputDirectory, "rebar.config");
    RebarEunitRunningState.writeModifiedConfig(getProject(), oldConfig, newConfig);
    FileDocumentManager.getInstance().saveAllDocuments();
    assertTrue(newConfig.exists());
    return FileUtil.loadFile(newConfig);
  }
}
