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

import com.intellij.application.options.ModulesComboBox;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.util.Disposer;
import com.intellij.util.ui.UIUtil;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.console.ErlangConsoleRunConfigurationType;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfigurationType;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

public class ErlangSmallIdeRunConfigurationEditorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangSmallIdeRunConfigurationEditorTest() {
    super(true);
  }

  public void testApplicationConfigurationModuleCanBeChosen() {
    doTestModuleCanBeChosen(ErlangApplicationRunConfigurationType.getInstance().getConfigurationFactories()[0]
                              .createTemplateConfiguration(getProject()));
  }

  public void testEunitConfigurationModuleCanBeChosen() {
    doTestModuleCanBeChosen(ErlangUnitRunConfigurationType.getInstance().getConfigurationFactories()[0]
                              .createTemplateConfiguration(getProject()));
  }

  public void testRemoteDebugConfigurationModuleCanBeChosen() {
    doTestModuleCanBeChosen(ErlangRemoteDebugRunConfigurationType.getInstance().getConfigurationFactories()[0]
                              .createTemplateConfiguration(getProject()));
  }

  public void testConsoleConfigurationModuleCanBeChosen() {
    doTestModuleCanBeChosen(ErlangConsoleRunConfigurationType.getInstance().getConfigurationFactories()[0]
                              .createTemplateConfiguration(getProject()), JComboBox.class);
  }

  private void doTestModuleCanBeChosen(RunConfiguration configuration) {
    doTestModuleCanBeChosen(configuration, ModulesComboBox.class);
  }

  @SuppressWarnings("unchecked")
  private void doTestModuleCanBeChosen(RunConfiguration configuration, Class<? extends JComboBox> comboBoxClass) {
    assertTrue(ErlangSystemUtil.isSmallIde());
    SettingsEditor<RunConfiguration> editor = (SettingsEditor<RunConfiguration>) configuration.getConfigurationEditor();
    try {
      JComponent component = editor.getComponent();
      editor.resetFrom(configuration);
      JComboBox<?> comboBox = UIUtil.findComponentOfType(component, comboBoxClass);
      assertNotNull(comboBox);
      List<Module> choices = new ArrayList<>();
      for (int i = 0; i < comboBox.getModel().getSize(); i++) {
        Module module = (Module) comboBox.getModel().getElementAt(i);
        if (module != null) choices.add(module);
      }
      assertContainsElements(choices, getModule());
    }
    finally {
      Disposer.dispose(editor);
    }
  }
}
