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
import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.ConfigurationFromContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformCoreDataKeys;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.testFramework.MapDataContext;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationProducer;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfigurationType;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunningState;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangRunConfigurationArgumentsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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

  public void testApplicationErlFlagWithQuotedSpace() throws ExecutionException {
    assertErlFlags("-pa \"/Users/John Smith/ebin\" +P 1000", "-pa", "/Users/John Smith/ebin", "+P", "1000");
  }

  public void testApplicationErlFlagsAreSplitOnSpaces() throws ExecutionException {
    assertErlFlags("+P 1000  -sname node", "+P", "1000", "-sname", "node");
  }

  // #1334: a function whose name needs quotes must be called by its quoted atom in -eval.
  public void testEntryPointWithQuotedFunctionName() throws ExecutionException {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['my-func'/0]).
      'my-f<caret>unc'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();

    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    List<String> parameters = state.getCommand().getParametersList().getList();
    int eval = parameters.indexOf("-eval");
    assertTrue(parameters.toString(), eval >= 0 && eval + 1 < parameters.size());
    assertEquals("m:'my-func'().", parameters.get(eval + 1));
  }

  public void testEntryPointWithSpaceInFunctionName() throws Exception {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['hello world'/0]).
      'hello w<caret>orld'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();
    configuration.checkConfiguration();

    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    List<String> parameters = state.getCommand().getParametersList().getList();
    int eval = parameters.indexOf("-eval");
    assertTrue(parameters.toString(), eval >= 0 && eval + 1 < parameters.size());
    assertEquals("m:'hello world'().", parameters.get(eval + 1));
  }

  public void testEntryPointWithReservedWordFunctionName() throws Exception {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['end'/0]).
      'e<caret>nd'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();
    configuration.checkConfiguration();
    assertEquals("m:'end'().", getEval(configuration));
  }

  public void testTypedQuotedReservedWordFunctionName() throws Exception {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['receive'/0]).
      'receive'() -> ok.
      """);
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModule(getModule());
    configuration.setModuleAndFunction("m 'receive'");
    configuration.checkConfiguration();
    assertEquals("m:'receive'().", getEval(configuration));
  }

  public void testEntryPointWithEmptyFunctionName() throws Exception {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export([''/0]).
      '<caret>'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();
    configuration.checkConfiguration();
    assertEquals("m:''().", getEval(configuration));
  }

  public void testEntryPointWithEscapeInFunctionName() throws Exception {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['it\\'s'/0]).
      'i<caret>t\\'s'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();
    configuration.checkConfiguration();
    assertEquals("m:'it\\'s'().", getEval(configuration));

    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    ErlangRunningState.ErlangEntryPoint entryPoint = state.getDebugEntryPoint();
    assertEquals("m", entryPoint.getModuleName());
    assertEquals("it's", entryPoint.getFunctionName());
  }

  public void testCheckConfigurationDoesNotLoadModuleAst() throws Exception {
    PsiFileImpl file = (PsiFileImpl) myFixture.addFileToProject("m2.erl", """
      -module(m2).
      -export(['my-func'/0]).
      'my-func'() -> ok.
      """);
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModule(getModule());
    configuration.setModuleAndFunction("m2 'my-func'");
    assertFalse(file.isContentsLoaded());
    configuration.checkConfiguration();
    assertFalse(file.isContentsLoaded());
  }

  public void testConfigurationNameKeepsEscapes() {
    myFixture.configureByText("m.erl", """
      -module(m).
      -export(['a\\nb'/0]).
      'a<caret>\\nb'() -> ok.
      """);
    ErlangApplicationConfiguration configuration = createConfigurationAtCaret();
    assertEquals("m.a\\nb", configuration.getName());
    assertEquals("m 'a\\nb'", configuration.getModuleAndFunction());
  }

  private String getEval(ErlangApplicationConfiguration configuration) throws ExecutionException {
    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    List<String> parameters = state.getCommand().getParametersList().getList();
    int eval = parameters.indexOf("-eval");
    assertTrue(parameters.toString(), eval >= 0 && eval + 1 < parameters.size());
    return parameters.get(eval + 1);
  }

  private ErlangApplicationConfiguration createConfigurationAtCaret() {
    PsiElement element = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    assertNotNull(element);
    MapDataContext dataContext = new MapDataContext();
    dataContext.put(CommonDataKeys.PROJECT, getProject());
    dataContext.put(PlatformCoreDataKeys.MODULE, getModule());
    dataContext.put(CommonDataKeys.PSI_ELEMENT, element);
    dataContext.put(Location.DATA_KEY, PsiLocation.fromPsiElement(element));
    ConfigurationContext context = ConfigurationContext.getFromContext(dataContext, null);
    ConfigurationFromContext fromContext =
      RunConfigurationProducer.getInstance(ErlangApplicationRunConfigurationProducer.class).createConfigurationFromContext(context);
    assertNotNull(fromContext);
    assertInstanceOf(fromContext.getConfiguration(), ErlangApplicationConfiguration.class);
    return (ErlangApplicationConfiguration) fromContext.getConfiguration();
  }

  // #1352: a remote node configuration whose debug node args were never set means no extra arguments
  public void testRemoteDebugTemplateWithoutDebugNodeArgs() {
    ErlangRemoteDebugRunConfiguration configuration = (ErlangRemoteDebugRunConfiguration)
      ErlangRemoteDebugRunConfigurationType.getInstance().getConfigurationFactories()[0].createTemplateConfiguration(getProject());
    configuration.setRemoteErlangNodeName("smoke_target@host");
    configuration.setCookie("smokecookie");
    assertRemoteDebugNodeFlagsOnlyName(configuration);
  }

  public void testRemoteDebugReadWithoutDebugNodeArgs() throws Exception {
    ErlangRemoteDebugRunConfiguration configuration = new ErlangRemoteDebugRunConfiguration(getProject(), "remote smoke_target");
    configuration.readExternal(JDOMUtil.load(
      "<configuration name=\"remote smoke_target\" type=\"ErlangRemoteDebugRunConfiguration\" factoryName=\"Erlang Remote Node\">" +
      "<option name=\"remoteErlangNodeName\" value=\"smoke_target@host\" />" +
      "<option name=\"cookie\" value=\"smokecookie\" />" +
      "</configuration>"));
    assertRemoteDebugNodeFlagsOnlyName(configuration);
  }

  private void assertRemoteDebugNodeFlagsOnlyName(ErlangRemoteDebugRunConfiguration configuration) {
    ErlangRemoteDebugRunningState state = new ErlangRemoteDebugRunningState(null, getModule(), configuration);
    GeneralCommandLine commandLine = new GeneralCommandLine();
    state.setErlangFlags(commandLine);
    List<String> parameters = commandLine.getParametersList().getList();
    assertEquals(parameters.toString(), 2, parameters.size());
    assertEquals("-sname", parameters.get(0));
    assertTrue(parameters.toString(), parameters.get(1).startsWith("debugger_node_"));
  }

  private void assertErlFlags(String erlFlags, String... expected) throws ExecutionException {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModuleAndFunction("sample start");
    configuration.setErlFlags(erlFlags);

    ErlangRunningState state = new ErlangApplicationRunningState(null, getModule(), configuration);
    List<String> parameters = state.getCommand().getParametersList().getList();
    // The erl flags come right before the entry point (#616).
    int eval = parameters.indexOf("-eval");
    assertTrue(parameters.toString(), eval >= expected.length);
    assertEquals(List.of(expected), parameters.subList(eval - expected.length, eval));
  }
}
