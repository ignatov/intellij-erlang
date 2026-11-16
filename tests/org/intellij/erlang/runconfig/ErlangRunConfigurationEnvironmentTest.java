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
import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.application.ui.ErlangRunConfigurationEditorForm;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunningState;
import org.intellij.erlang.eunit.ui.ErlangUnitRunConfigurationEditorForm;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jdom.Element;

import java.util.Map;
import java.util.Set;

public class ErlangRunConfigurationEnvironmentTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final EnvironmentVariablesData ENV_DATA =
    EnvironmentVariablesData.create(Map.of("ERL_LIBS", "/opt/erlang/libs"), false);

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

  public void testApplicationEnvironmentIsSerialized() {
    ErlangApplicationConfiguration configuration = createApplicationConfiguration();
    configuration.setErlFlags("+P 1000");
    configuration.setEnvData(ENV_DATA);

    ErlangApplicationConfiguration restored = createApplicationConfiguration();
    restored.readExternal(writeExternal(configuration));

    assertEquals(ENV_DATA, restored.getEnvData());
    assertEquals("+P 1000", restored.getErlFlags());
  }

  public void testUnitEnvironmentIsSerialized() {
    ErlangUnitRunConfiguration configuration = createUnitConfiguration();
    configuration.getConfigData().setModuleNames(Set.of("sample_tests"));
    configuration.setEnvData(ENV_DATA);

    ErlangUnitRunConfiguration restored = createUnitConfiguration();
    restored.readExternal(writeExternal(configuration));

    assertEquals(ENV_DATA, restored.getEnvData());
    assertEquals(Set.of("sample_tests"), restored.getConfigData().getModuleNames());
  }

  public void testDefaultEnvironmentIsNotWritten() {
    Element element = writeExternal(createApplicationConfiguration());
    assertNull(element.getChild("envs"));
    assertFalse(JDOMUtil.write(element).contains("envData"));

    ErlangApplicationConfiguration restored = createApplicationConfiguration();
    restored.readExternal(element);
    assertEquals(EnvironmentVariablesData.DEFAULT, restored.getEnvData());
  }

  public void testApplicationEditorKeepsEnvironment() throws ConfigurationException {
    ErlangApplicationConfiguration configuration = createApplicationConfiguration();
    configuration.setEnvData(ENV_DATA);
    ErlangApplicationConfiguration edited = createApplicationConfiguration();

    applyThroughEditor(new ErlangRunConfigurationEditorForm(), configuration, edited);
    assertEquals(ENV_DATA, edited.getEnvData());
  }

  public void testUnitEditorKeepsEnvironment() throws ConfigurationException {
    ErlangUnitRunConfiguration configuration = createUnitConfiguration();
    configuration.setEnvData(ENV_DATA);
    ErlangUnitRunConfiguration edited = createUnitConfiguration();

    applyThroughEditor(new ErlangUnitRunConfigurationEditorForm(), configuration, edited);
    assertEquals(ENV_DATA, edited.getEnvData());
  }

  public void testApplicationCommandLineEnvironment() throws ExecutionException {
    ErlangApplicationConfiguration configuration = createApplicationConfiguration();
    configuration.setModuleAndFunction("sample start");
    configuration.setEnvData(ENV_DATA);

    assertEnvironment(getCommand(new ErlangApplicationRunningState(null, getModule(), configuration)));
  }

  public void testUnitCommandLineEnvironment() throws ExecutionException {
    ErlangUnitRunConfiguration configuration = createUnitConfiguration();
    configuration.getConfigData().setModuleNames(Set.of("sample_tests"));
    configuration.setEnvData(ENV_DATA);

    assertEnvironment(getCommand(new ErlangUnitRunningState(null, getModule(), configuration)));
  }

  public void testDefaultCommandLineEnvironment() throws ExecutionException {
    ErlangApplicationConfiguration configuration = createApplicationConfiguration();
    configuration.setModuleAndFunction("sample start");

    GeneralCommandLine commandLine = getCommand(new ErlangApplicationRunningState(null, getModule(), configuration));
    assertEmpty(commandLine.getEnvironment().keySet());
    assertEquals(GeneralCommandLine.ParentEnvironmentType.CONSOLE, commandLine.getParentEnvironmentType());
  }

  private static void assertEnvironment(GeneralCommandLine commandLine) {
    assertEquals(Map.of("ERL_LIBS", "/opt/erlang/libs"), commandLine.getEnvironment());
    assertEquals(GeneralCommandLine.ParentEnvironmentType.NONE, commandLine.getParentEnvironmentType());
  }

  private static <T extends ErlangRunConfigurationBase<?>> void applyThroughEditor(SettingsEditor<T> editor, T from, T to)
    throws ConfigurationException {
    try {
      editor.resetFrom(from);
      editor.applyTo(to);
    }
    finally {
      Disposer.dispose(editor);
    }
  }

  private static GeneralCommandLine getCommand(ErlangRunningState state) throws ExecutionException {
    return state.getCommand();
  }

  private static Element writeExternal(ErlangRunConfigurationBase<?> configuration) {
    Element element = new Element("configuration");
    configuration.writeExternal(element);
    return element;
  }

  private ErlangApplicationConfiguration createApplicationConfiguration() {
    return new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
  }

  private ErlangUnitRunConfiguration createUnitConfiguration() {
    return new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
  }
}
