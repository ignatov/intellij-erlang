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

import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Map;

public class RebarEunitRunningStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String REPORT_OPTION = "{report,{" + ErlangEunitReporterModule.MODULE_NAME + ", []}}";
  private static final String NO_TTY_OPTION = "{no_tty, true}";
  private static final String MODIFIED_CONFIG =
    "{erl_opts, [debug_info]}.\n{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ",verbose]}.\n";

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testErlFlagsQuoteReportsDirectoryWithSpace() {
    File reportsRoot = new File(FileUtil.getTempDirectory(), "John Smith/eunit reports");
    String erlFlags = getErlFlags(null, reportsRoot);
    assertTrue(erlFlags, erlFlags.startsWith("-pa \"" + FileUtil.toSystemIndependentName(reportsRoot.getPath()) + "\""));
  }

  public void testErlFlagsKeepUserErlFlags() {
    File reportsRoot = new File(FileUtil.getTempDirectory(), "eunit");
    String erlFlags = getErlFlags("+P 1000 -extra foo", reportsRoot);
    assertEquals("-pa \"" + FileUtil.toSystemIndependentName(reportsRoot.getPath()) + "\" +P 1000 -extra foo", erlFlags);
  }

  public void testErlFlagsEscapeDoubleQuote() {
    File reportsRoot = new File("/tmp/a\"b");
    assertEquals("-pa \"/tmp/a\"\\\"\"b\" +P 1000", getErlFlags("+P 1000", reportsRoot));
  }

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

  public void testWriteModifiedConfigRemovesReportBetweenOtherOptions() throws Exception {
    String config = doWriteModifiedConfig(
      "{eunit_opts, [verbose, {report, {eunit_surefire, [{dir, \".\"}]}}, {print_depth, 10}]}.\n");
    assertEquals("{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ",verbose, {print_depth, 10}]}.\n", config);
  }

  public void testWriteModifiedConfigRemovesNoTtyBetweenOtherOptions() throws Exception {
    String config = doWriteModifiedConfig("{eunit_opts, [verbose, {no_tty, false}, {print_depth, 10}]}.\n");
    assertEquals("{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ",verbose, {print_depth, 10}]}.\n", config);
  }

  public void testWriteModifiedConfigRemovesAdjacentReportAndNoTtyBetweenOtherOptions() throws Exception {
    String config = doWriteModifiedConfig("{eunit_opts, [a, {report, x}, {no_tty, true}, b]}.\n");
    assertEquals("{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ",a, b]}.\n", config);
  }

  public void testWriteModifiedConfigRemovesFirstAndLastReportAndNoTty() throws Exception {
    String config = doWriteModifiedConfig("{eunit_opts, [{report, x}, a, {no_tty, true}]}.\n");
    assertEquals("{eunit_opts, [" + NO_TTY_OPTION + "," + REPORT_OPTION + ", a]}.\n", config);
  }

  public void testWriteModifiedConfigWithoutOldConfig() throws Exception {
    String config = doWriteModifiedConfig(null);
    assertTrue(config, config.contains(REPORT_OPTION));
    assertTrue(config, config.contains(NO_TTY_OPTION));
  }

  // Without a project SDK the release is unknown, so the run gets the reporter that OTP 26 and later load.
  public void testReporterModuleWithoutProjectSdk() throws Exception {
    Sdk sdk = ProjectRootManager.getInstance(getProject()).getProjectSdk();
    WriteAction.run(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
    try {
      assertReporterModule("otp_27_plus");
    }
    finally {
      WriteAction.run(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk));
    }
  }

  public void testReporterModuleOfOlderProjectSdk() throws Exception {
    assertReporterModule("otp_pre_27");
  }

  private void assertReporterModule(String beamDirectory) throws Exception {
    File projectDirectory = FileUtil.createTempDirectory("rebar_project", null);
    File reportsRoot = RebarEunitRunningState.createEunitReportsEnvironment(getProject(), projectDirectory.getPath());
    FileDocumentManager.getInstance().saveAllDocuments();
    byte[] actual = Files.readAllBytes(new File(reportsRoot, ErlangEunitReporterModule.MODULE_NAME + ".beam").toPath());
    try (InputStream expected = getClass().getResourceAsStream("/eunit/beams/" + beamDirectory + "/eunit_teamcity.beam")) {
      assertNotNull(expected);
      assertTrue(beamDirectory, Arrays.equals(expected.readAllBytes(), actual));
    }
  }

  private String getErlFlags(String userErlFlags, File reportsRoot) {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    if (userErlFlags != null) {
      configuration.setEnvData(EnvironmentVariablesData.create(Map.of("ERL_FLAGS", userErlFlags), true));
    }
    return RebarEunitRunningState.getCommandLine(configuration, reportsRoot).getEnvironment().get("ERL_FLAGS");
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
