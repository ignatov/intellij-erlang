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

package org.intellij.erlang.console;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

public class ErlangConsoleCommandLineStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  public void testConsoleArgWithQuotedSpace() throws ExecutionException {
    assertConsoleArgs("-pa \"C:\\Program Files\\app\\ebin\" -sname node",
                      "-pa", "C:\\Program Files\\app\\ebin", "-sname", "node");
  }

  public void testConsoleArgsAreSplitOnSpaces() throws ExecutionException {
    assertConsoleArgs("-sname node  +P 1000", "-sname", "node", "+P", "1000");
  }

  public void testConsoleStartsWithUtf8Locale() throws ExecutionException {
    ErlangConsoleRunConfiguration configuration = new ErlangConsoleRunConfiguration("console", getProject());
    configuration.getConfigurationModule().setModule(getModule());

    GeneralCommandLine commandLine = ErlangConsoleCommandLineState.createCommandLine(configuration);
    assertEquals(StandardCharsets.UTF_8, commandLine.getCharset());
    String ctype = ErlangConsoleCommandLineState.getLocaleCharacterType(commandLine.getEffectiveEnvironment());
    assertTrue(ctype, ErlangConsoleCommandLineState.isUtf8Locale(ctype));
  }

  public void testNonUtf8LocaleIsReplaced() {
    GeneralCommandLine commandLine = commandLineWithParentEnvironment(Map.of("LANG", "C"));
    ErlangConsoleCommandLineState.setUtf8Locale(commandLine);
    assertTrue(commandLine.getEnvironment().get("LC_CTYPE"), commandLine.getEnvironment().get("LC_CTYPE").endsWith(".UTF-8"));
  }

  public void testMissingLocaleIsReplaced() {
    GeneralCommandLine commandLine = commandLineWithParentEnvironment(Map.of());
    ErlangConsoleCommandLineState.setUtf8Locale(commandLine);
    assertTrue(commandLine.getEnvironment().get("LC_CTYPE"), commandLine.getEnvironment().get("LC_CTYPE").endsWith(".UTF-8"));
  }

  public void testNonUtf8LcAllIsReplaced() {
    GeneralCommandLine commandLine = commandLineWithParentEnvironment(Map.of("LC_ALL", "C", "LC_CTYPE", "sv_SE.UTF-8"));
    ErlangConsoleCommandLineState.setUtf8Locale(commandLine);
    Map<String, String> environment = commandLine.getEnvironment();
    assertTrue(environment.get("LC_ALL"), environment.get("LC_ALL").endsWith(".UTF-8"));
    assertEquals(environment.get("LC_ALL"), environment.get("LC_CTYPE"));
  }

  public void testUtf8LocaleIsKept() {
    GeneralCommandLine commandLine = commandLineWithParentEnvironment(Map.of("LANG", "C", "LC_CTYPE", "sv_SE.utf8"));
    ErlangConsoleCommandLineState.setUtf8Locale(commandLine);
    assertEquals(Map.of("LANG", "C", "LC_CTYPE", "sv_SE.utf8"), commandLine.getEnvironment());
  }

  private static GeneralCommandLine commandLineWithParentEnvironment(Map<String, String> environment) {
    // The variables stand for the inherited environment of the IDE, which the test cannot change.
    return new GeneralCommandLine("erl")
      .withParentEnvironmentType(GeneralCommandLine.ParentEnvironmentType.NONE)
      .withEnvironment(environment);
  }

  private void assertConsoleArgs(String consoleArgs, String... expected) throws ExecutionException {
    ErlangConsoleRunConfiguration configuration = new ErlangConsoleRunConfiguration("console", getProject());
    configuration.getConfigurationModule().setModule(getModule());
    configuration.setConsoleArgs(consoleArgs);

    List<String> parameters = ErlangConsoleCommandLineState.createCommandLine(configuration).getParametersList().getList();
    // The console arguments are the first parameters of the command line, the code path follows them.
    assertEquals(List.of(expected), parameters.subList(0, expected.length));
    assertEquals(ErlangConsoleUtil.getCodePath(getProject(), getModule(), false),
                 parameters.subList(expected.length, parameters.size()));
  }
}
