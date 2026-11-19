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
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;
import java.util.List;
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

  public void testRebar3CommandSelectsModuleAndTest() {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    try {
      rebarSettings.setRebarPath("/opt/rebar/rebar3");
      ErlangFile file = (ErlangFile) myFixture.configureByText("b.erl", "-module(b).\nb_test() -> ok.\nb2_test() -> ok.\n");
      ErlangFunction function = file.getFunction("b_test", 0);
      assertNotNull(function);

      RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "b:b_test");
      configuration.setCommand(RebarEunitConfigurationUtil.createDefaultRebarCommand(List.of(file), List.of(function), true));
      List<String> parameters =
        RebarEunitRunningState.getCommandLine(configuration, new File(FileUtil.getTempDirectory(), "eunit")).getParametersList().getList();

      assertFalse(parameters.toString(), parameters.contains("suites=b"));
      assertFalse(parameters.toString(), parameters.contains("tests=b_test"));
      assertTrue(parameters.toString(), parameters.contains("--test=b:b_test") ||
                                        parameters.indexOf("--test") >= 0 && parameters.contains("b:b_test"));
    }
    finally {
      rebarSettings.setRebarPath(oldRebarPath);
    }
  }

  public void testRebar3CommandSelectsModule() {
    useRebar("rebar3");
    ErlangFile file = (ErlangFile) myFixture.configureByText("b.erl", "-module(b).\nb_test() -> ok.\n");
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(List.of(file), List.of(), true);
    assertEquals("eunit --module=b", command);
    assertEquals(List.of("eunit", "--module=b"), getParameters(command));
  }

  public void testRebar3CommandNamesTestsWithTheirModules() {
    useRebar("rebar3");
    ErlangFile a = (ErlangFile) myFixture.addFileToProject("a.erl", "-module(a).\na2_test() -> ok.\n");
    ErlangFile b = (ErlangFile) myFixture.addFileToProject("b.erl", "-module(b).\nb_test() -> ok.\n");
    List<ErlangFunction> functions = List.of(a.getFunction("a2_test", 0), b.getFunction("b_test", 0));
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(List.of(a, b), functions, false);
    assertEquals("eunit --test=a:a2_test,b:b_test", command);
    assertEquals(List.of("eunit", "--test=a:a2_test,b:b_test"), getParameters(command));
  }

  public void testRebar3RunsSavedRebar2Command() {
    useRebar("rebar3");
    assertEquals(List.of("eunit", "--test=b:b_test,b:b2_test"), getParameters("eunit suites=b tests=b_test,b2_test"));
    assertEquals(List.of("eunit", "--module=a,b"), getParameters("eunit suites=a,b tests=b_test"));
    assertEquals(List.of("eunit", "--module=b"), getParameters("eunit suites=b "));
    assertEquals(List.of("eunit", "--test=b:b_test", "--generator=b:b_test_"), getParameters("eunit suites=b tests=b_test,b_test_"));
  }

  public void testRebar3CommandRunsGeneratorWithGeneratorOption() {
    useRebar("rebar3");
    ErlangFile file = (ErlangFile) myFixture.configureByText("b.erl", "-module(b).\nb_test_() -> [].\n");
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(List.of(file), List.of(file.getFunction("b_test_", 0)), true);
    assertEquals("eunit --generator=b:b_test_", command);
    assertEquals(List.of("eunit", "--generator=b:b_test_"), getParameters(command));
  }

  public void testRebar2CommandUnchanged() {
    useRebar("rebar");
    ErlangFile file = (ErlangFile) myFixture.configureByText("b.erl", "-module(b).\nb_test() -> ok.\n");
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(List.of(file), List.of(file.getFunction("b_test", 0)), true);
    assertEquals("eunit suites=b tests=b_test", command);
    assertEquals(List.of("skip_deps=true", "eunit", "suites=b", "tests=b_test", "-C"), getParameters(command).subList(0, 5));
  }

  private void useRebar(String rebarName) {
    RebarSettings rebarSettings = RebarSettings.getInstance(getProject());
    String oldRebarPath = rebarSettings.getRebarPath();
    rebarSettings.setRebarPath("/opt/rebar/" + rebarName);
    Disposer.register(getTestRootDisposable(), () -> rebarSettings.setRebarPath(oldRebarPath));
  }

  private List<String> getParameters(String command) {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "b");
    configuration.setCommand(command);
    configuration.setSkipDependencies(true);
    List<String> parameters =
      RebarEunitRunningState.getCommandLine(configuration, new File(FileUtil.getTempDirectory(), "eunit")).getParametersList().getList();
    return parameters.subList(1, parameters.size());
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

  public void testWriteModifiedConfigWithoutOldConfig() throws Exception {
    String config = doWriteModifiedConfig(null);
    assertTrue(config, config.contains(REPORT_OPTION));
    assertTrue(config, config.contains(NO_TTY_OPTION));
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
