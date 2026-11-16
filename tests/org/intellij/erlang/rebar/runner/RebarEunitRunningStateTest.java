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
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.eunit.ErlangEunitReporterModule;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

  public void testErlFlagsPutReporterDirectoryWithSpaceOnCodePath() throws Exception {
    String oldTempDirectory = FileUtil.getTempDirectory();
    File tempDirectoryWithSpace = new File(oldTempDirectory, "temp with space");
    assertTrue(tempDirectoryWithSpace.mkdirs());
    FileUtil.resetCanonicalTempPathCache(tempDirectoryWithSpace.getPath());
    String erlFlags;
    try {
      erlFlags = doStartProcessAndGetErlFlags(new HashMap<>());
    }
    finally {
      FileUtil.resetCanonicalTempPathCache(oldTempDirectory);
    }
    List<String> args = splitAsErlexec(erlFlags);
    assertReporterDirectoryOnCodePath(erlFlags, args);
  }

  public void testErlFlagsKeepUserErlFlags() throws Exception {
    Map<String, String> envs = new HashMap<>();
    envs.put("ERL_FLAGS", "-kernel foo bar");
    String erlFlags = doStartProcessAndGetErlFlags(envs);
    List<String> args = splitAsErlexec(erlFlags);
    int pa = assertReporterDirectoryOnCodePath(erlFlags, args);
    int kernel = args.indexOf("-kernel");
    assertTrue("the user's ERL_FLAGS are lost: ERL_FLAGS=" + erlFlags, kernel >= 0);
    assertEquals("ERL_FLAGS=" + erlFlags, List.of("-kernel", "foo", "bar"), args.subList(kernel, kernel + 3));
    assertTrue("-pa must come before the user's flags: ERL_FLAGS=" + erlFlags, pa < kernel);
  }

  // Returns the index of -pa in args.
  private static int assertReporterDirectoryOnCodePath(String erlFlags, List<String> args) {
    int pa = args.indexOf("-pa");
    assertTrue("no -pa in ERL_FLAGS=" + erlFlags, pa >= 0 && pa + 1 < args.size());
    File reporterBeam = new File(args.get(pa + 1), ErlangEunitReporterModule.MODULE_NAME + ".beam");
    assertTrue("erl would get -pa " + args.get(pa + 1) + ", which has no " + reporterBeam.getName() +
               "; ERL_FLAGS=" + erlFlags + ", erlexec arguments=" + args, reporterBeam.isFile());
    return pa;
  }

  // Splits ERL_FLAGS into arguments the way erlexec does: on whitespace, with double-quoted strings
  // taken literally and a backslash outside quotes escaping the next character.
  private static List<String> splitAsErlexec(String flags) {
    List<String> args = new ArrayList<>();
    StringBuilder current = null;
    boolean quoted = false;
    for (int i = 0; i < flags.length(); i++) {
      char c = flags.charAt(i);
      if (!quoted && Character.isWhitespace(c)) {
        if (current != null) args.add(current.toString());
        current = null;
        continue;
      }
      if (current == null) current = new StringBuilder();
      if (c == '"') quoted = !quoted;
      else if (c == '\\' && !quoted && i + 1 < flags.length()) current.append(flags.charAt(++i));
      else current.append(c);
    }
    if (current != null) args.add(current.toString());
    return args;
  }

  // Runs RebarEunitRunningState.startProcess with an Erlang SDK whose escript is a shell script that writes
  // the ERL_FLAGS it was started with to a file, and returns them.
  private String doStartProcessAndGetErlFlags(Map<String, String> userEnvs) throws Exception {
    File sdkHome = FileUtil.createTempDirectory("fake_erlang_sdk", null);
    File escript = new File(sdkHome, "bin/escript");
    File output = new File(sdkHome, "erl_flags.txt");
    FileUtil.writeToFile(escript, "#!/bin/sh\nprintf '%s' \"$ERL_FLAGS\" > \"$ERL_FLAGS_OUTPUT\"\n");
    assertTrue(escript.setExecutable(true));
    assertTrue(new File(sdkHome, "lib").mkdir());
    // The process runs in the project base path of the light test project, which is not created on disk.
    File workingDirectory = new File(RebarRunningStateUtil.getWorkingDirectory(new RebarEunitRunConfiguration(getProject(), "eunit")));
    assertTrue(workingDirectory.isDirectory() || workingDirectory.mkdirs());
    Sdk sdk = ErlangSdkType.createMockSdk(sdkHome.getPath(), ErlangSdkRelease.V_R15B02);
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable.getInstance().addJdk(sdk);
      ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
    });
    try {
      Map<String, String> envs = new HashMap<>(userEnvs);
      envs.put("ERL_FLAGS_OUTPUT", output.getPath());
      RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
      configuration.setEnvData(EnvironmentVariablesData.create(envs, true));
      ExecutionEnvironment environment =
        ExecutionEnvironmentBuilder.create(DefaultRunExecutor.getRunExecutorInstance(), configuration).build();
      ProcessHandler handler = new RebarEunitRunningState(environment, configuration).startProcess();
      handler.startNotify();
      assertTrue(handler.waitFor(30000));
      assertTrue(output.isFile());
      return FileUtil.loadFile(output);
    }
    finally {
      ApplicationManager.getApplication().runWriteAction(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
    }
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
