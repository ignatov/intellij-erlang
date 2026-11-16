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
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

public class RebarRunningStateUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final List<String> WINDOWS_EXTENSIONS = List.of(".com", ".exe", ".bat", ".cmd");

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

  public void testCommandArgumentWithQuotedSpace() {
    assertRebarCommand("ct --dir \"/Users/John Smith/test\" --verbose", "ct", "--dir", "/Users/John Smith/test", "--verbose");
  }

  public void testCommandIsSplitOnSpaces() {
    assertRebarCommand("eunit  --module=sample_tests", "eunit", "--module=sample_tests");
  }

  private void assertRebarCommand(String command, String... expected) {
    RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
    configuration.setCommand(command);

    List<String> parameters = RebarRunningStateUtil.getRebarCommandLine(configuration).getParametersList().getList();
    // The rebar escript is the first parameter, the rebar command follows it.
    assertEquals(RebarSettings.getInstance(getProject()).getRebarEscriptPath(), parameters.get(0));
    assertEquals(List.of(expected), parameters.subList(1, parameters.size()));
  }

  public void testCommandLineWithoutParentEnvironment() {
    GeneralCommandLine commandLine = getCommandLineWithEnvironment(false);
    assertEquals(GeneralCommandLine.ParentEnvironmentType.NONE, commandLine.getParentEnvironmentType());
    assertEquals("bar", commandLine.getEffectiveEnvironment().get("FOO"));
  }

  public void testCommandLineWithParentEnvironment() {
    GeneralCommandLine commandLine = getCommandLineWithEnvironment(true);
    assertEquals(GeneralCommandLine.ParentEnvironmentType.CONSOLE, commandLine.getParentEnvironmentType());
    assertEquals("bar", commandLine.getEffectiveEnvironment().get("FOO"));
  }

  private GeneralCommandLine getCommandLineWithEnvironment(boolean passParentEnvs) {
    RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
    configuration.setCommand("compile");
    configuration.setEnvData(EnvironmentVariablesData.create(Map.of("FOO", "bar"), passParentEnvs));
    return RebarRunningStateUtil.getRebarCommandLine(configuration);
  }

  public void testEscriptOfProjectSdk() {
    String sdkPath = ErlangSdkType.getSdkPath(getProject());
    assertNotNull(sdkPath);
    assertEquals(JpsErlangSdkType.getScriptInterpreterExecutable(sdkPath).getAbsolutePath(),
                 RebarRunningStateUtil.findEscriptExecutable(getProject()));
  }

  public void testCommandLineRunsEscriptNextToWrapper() throws IOException {
    File directory = FileUtil.createTempDirectory("rebar_bin", null);
    File rebar3 = createExecutable(directory, "rebar3");
    RebarSettings settings = RebarSettings.getInstance(getProject());
    String oldRebarPath = settings.getRebarPath();
    try {
      settings.setRebarPath(createExecutable(directory, "rebar3.cmd").getPath());
      RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
      configuration.setCommand("compile");
      configuration.setSkipDependencies(true);
      // The escript next to the wrapper, and no skip_deps=true, which is for rebar 2 only.
      assertEquals(List.of(rebar3.getPath(), "compile"),
                   RebarRunningStateUtil.getRebarCommandLine(configuration).getParametersList().getList());
    }
    finally {
      settings.setRebarPath(oldRebarPath);
    }
  }

  public void testFindInPathWithWindowsExtension() throws IOException {
    File directory = FileUtil.createTempDirectory("otp_bin", null);
    File escript = createExecutable(directory, "escript.exe");
    assertEquals(escript, RebarRunningStateUtil.findInPath("escript", directory.getPath(), WINDOWS_EXTENSIONS));
  }

  public void testFindInPathPrefersNameWithoutExtension() throws IOException {
    File directory = FileUtil.createTempDirectory("rebar_bin", null);
    createExecutable(directory, "rebar3.cmd");
    File rebar3 = createExecutable(directory, "rebar3");
    assertEquals(rebar3, RebarRunningStateUtil.findInPath("rebar3", directory.getPath(), WINDOWS_EXTENSIONS));
  }

  public void testFindInPathSearchesEveryDirectory() throws IOException {
    File empty = FileUtil.createTempDirectory("empty_bin", null);
    File directory = FileUtil.createTempDirectory("otp_bin", null);
    File escript = createExecutable(directory, "escript");
    String path = empty.getPath() + File.pathSeparator + directory.getPath();
    assertEquals(escript, RebarRunningStateUtil.findInPath("escript", path, List.of()));
  }

  public void testFindInPathNotFound() throws IOException {
    File directory = FileUtil.createTempDirectory("otp_bin", null);
    createExecutable(directory, "escript.exe");
    assertNull(RebarRunningStateUtil.findInPath("escript", directory.getPath(), List.of()));
    assertNull(RebarRunningStateUtil.findInPath("escript", null, WINDOWS_EXTENSIONS));
  }

  private static File createExecutable(File directory, String name) throws IOException {
    File file = new File(directory, name);
    FileUtil.writeToFile(file, "");
    FileUtil.setExecutable(file);
    return file;
  }
}
