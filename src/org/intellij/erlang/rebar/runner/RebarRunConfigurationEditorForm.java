/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.application.options.ModulesComboBox;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

final class RebarRunConfigurationEditorForm extends SettingsEditor<RebarRunConfigurationBase> {
  private JPanel myComponent;
  private JTextField myCommandText;
  private JCheckBox myRunInModuleCheckBox;
  private ModulesComboBox myModulesComboBox;
  private JCheckBox mySkipDependenciesCheckBox;

  RebarRunConfigurationEditorForm() {
    myRunInModuleCheckBox.addActionListener(e -> myModulesComboBox.setEnabled(myRunInModuleCheckBox.isSelected()));
    myModulesComboBox.setVisible(!ErlangSystemUtil.isSmallIde());
    myRunInModuleCheckBox.setVisible(!ErlangSystemUtil.isSmallIde());
  }

  @Override
  protected void resetEditorFrom(@NotNull RebarRunConfigurationBase configuration) {
    myCommandText.setText(configuration.getCommand());
    mySkipDependenciesCheckBox.setSelected(configuration.isSkipDependencies());
    Module module = null;
    if (!ErlangSystemUtil.isSmallIde()) {
      myModulesComboBox.fillModules(configuration.getProject(), ErlangModuleType.getInstance());
      module = configuration.getConfigurationModule().getModule();
    }
    if (module != null) {
      setRunInModuleSelected(true);
      myModulesComboBox.setSelectedModule(module);
    }
    else {
      setRunInModuleSelected(false);
    }
  }

  @Override
  protected void applyEditorTo(@NotNull RebarRunConfigurationBase rebarRunConfiguration) throws ConfigurationException {
    rebarRunConfiguration.setCommand(myCommandText.getText());
    rebarRunConfiguration.setSkipDependencies(mySkipDependenciesCheckBox.isSelected());
    Module selectedModule = myRunInModuleCheckBox.isSelected() ? myModulesComboBox.getSelectedModule() : null;
    rebarRunConfiguration.setModule(selectedModule);
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return myComponent;
  }

  @Override
  protected void disposeEditor() {
    myComponent.setVisible(false);
  }

  private void setRunInModuleSelected(boolean b) {
    if (b) {
      if (!myRunInModuleCheckBox.isSelected()) {
        myRunInModuleCheckBox.doClick();
      }
    }
    else {
      if (myRunInModuleCheckBox.isSelected()) {
        myRunInModuleCheckBox.doClick();
      }
    }
  }
}
