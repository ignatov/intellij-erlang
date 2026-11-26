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

package org.intellij.erlang.runconfig;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.impl.ConsoleViewImpl;
import com.intellij.execution.impl.EditorHyperlinkSupport;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.console.ErlangConsoleCommandLineState;
import org.intellij.erlang.console.ErlangConsoleRunConfiguration;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

// #965: the output of a run shows links to the .erl files it names
public class ErlangRunConsoleLinksTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String[] LINKED_LINES = {
    "src/a_module.erl:12: syntax error before: ')'",
    "{\"init terminating in do_boot\",{badarg,[{a_module,f,0,[{file,\"src/a_module.erl\"},{line,12}]}]}}",
    "in function a_module:f/0 (src/a_module.erl, line 12)",
  };

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    FileUtil.copyDir(new File("testData/rebar/sampleProject"), new File(getProject().getBasePath()));
    setUpProjectSdk();
  }

  public void testErlangApplicationRunConsole() throws Exception {
    Method createConsole = CommandLineState.class.getDeclaredMethod("createConsole", Executor.class);
    createConsole.setAccessible(true);
    assertLinksOnEveryLine((ConsoleView)createConsole.invoke(createApplicationState(), DefaultRunExecutor.getRunExecutorInstance()));
  }

  public void testErlangApplicationDebugConsole() {
    assertLinksOnEveryLine(createApplicationState().createConsoleView(DefaultRunExecutor.getRunExecutorInstance()));
  }

  public void testErlangConsole() throws ExecutionException {
    ErlangConsoleRunConfiguration configuration = new ErlangConsoleRunConfiguration("console", getProject());
    ExecutionEnvironment environment =
      ExecutionEnvironmentBuilder.create(getProject(), DefaultRunExecutor.getRunExecutorInstance(), configuration).build();
    assertLinksOnEveryLine(new ErlangConsoleCommandLineState(configuration, environment).getConsoleBuilder().getConsole());
  }

  @NotNull
  private ErlangApplicationRunningState createApplicationState() {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    return new ErlangApplicationRunningState(null, getModule(), configuration);
  }

  private void assertLinksOnEveryLine(@NotNull ConsoleView consoleView) {
    Disposer.register(getTestRootDisposable(), consoleView);
    ConsoleViewImpl console = (ConsoleViewImpl)consoleView;
    console.getComponent();
    for (String line : LINKED_LINES) {
      console.print(line + "\n", ConsoleViewContentType.NORMAL_OUTPUT);
    }
    console.flushDeferredText();
    console.waitAllRequests();
    EditorHyperlinkSupport hyperlinks = console.getHyperlinks();
    PlatformTestUtil.waitWithEventsDispatching(() -> "no link on: " + getLinesWithoutLink(hyperlinks),
                                               () -> getLinesWithoutLink(hyperlinks).isEmpty(), 10);
  }

  @NotNull
  private static List<String> getLinesWithoutLink(@NotNull EditorHyperlinkSupport hyperlinks) {
    List<String> lines = new ArrayList<>();
    for (int i = 0; i < LINKED_LINES.length; i++) {
      if (hyperlinks.findAllHyperlinksOnLine(i).isEmpty()) lines.add(LINKED_LINES[i]);
    }
    return lines;
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }
}
