/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.eunit.ui;

import com.intellij.ide.ui.ListCellRendererWrapper;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.intellij.erlang.editor.ErlangModuleType;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ErlangUnitRunConfigurationEditorForm extends SettingsEditor<ErlangUnitRunConfiguration> {
  private JPanel component;
  private JComboBox myComboModules;
  private JTextField myModuleAndFunctionField;

  @Override
  protected void resetEditorFrom(ErlangUnitRunConfiguration configuration) {
    myComboModules.removeAllItems();

    final Module[] modules = ModuleManager.getInstance(configuration.getProject()).getModules();
    for (final Module module : modules) {
      if (ModuleType.get(module) == ErlangModuleType.getInstance()) {
        myComboModules.addItem(module);
      }
    }
    myComboModules.setSelectedItem(configuration.getConfigurationModule().getModule());

    myComboModules.setRenderer(new ListCellRendererWrapper(myComboModules.getRenderer()) {
      @Override
      public void customize(JList list, Object value, int index, boolean selected, boolean hasFocus) {
        if (value instanceof Module) {
          final Module module = (Module) value;
          setText(module.getName());
        }
      }
    });

    myModuleAndFunctionField.setText(configuration.getModuleAndFunction());
  }

  @Override
  protected void applyEditorTo(ErlangUnitRunConfiguration configuration) throws ConfigurationException {
    configuration.setModule(getSelectedModule());
//    configuration.setParams(myParamsField.getText());
    configuration.setModuleAndFunction(myModuleAndFunctionField.getText());
  }

  private Module getSelectedModule() {
    return (Module) myComboModules.getSelectedItem();
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return component;
  }

  @Override
  protected void disposeEditor() {
    component.setVisible(false);
  }
}
