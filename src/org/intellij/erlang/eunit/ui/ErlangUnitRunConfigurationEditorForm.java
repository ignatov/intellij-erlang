/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.roots.ui.configuration.ModulesCombobox;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.ListCellRendererWrapper;
import org.intellij.erlang.ErlangModuleType;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class ErlangUnitRunConfigurationEditorForm extends SettingsEditor<ErlangUnitRunConfiguration> {
  private JPanel component;
  private ModulesCombobox myModuleComboBox;
  private JTextField myErlangModulesField;
  private JTextField myErlangFunctionsField;
  private JLabel myErlangModulesLabel;
  private JLabel myErlangFunctionsLabel;
  private JComboBox myTestKindComboBox;

  public ErlangUnitRunConfigurationEditorForm() {
    myTestKindComboBox.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent e) {
        onTestKindSwitch();
      }
    });
  }

  @Override
  protected void resetEditorFrom(ErlangUnitRunConfiguration configuration) {
    myModuleComboBox.fillModules(configuration.getProject(), ErlangModuleType.getInstance());
    myModuleComboBox.setSelectedModule(configuration.getConfigurationModule().getModule());

    ErlangUnitRunConfiguration.ErlangUnitConfigData configData = configuration.getConfigData();

    myTestKindComboBox.removeAllItems();
    ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind[] kinds = ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.values();
    for (ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind : kinds) {
      myTestKindComboBox.addItem(kind);
    }
    myTestKindComboBox.setSelectedItem(configData.getKind());
    myTestKindComboBox.setRenderer(getTestKindListCellRendererWrapper());

    myErlangModulesField.setText(getCommaSeparatedNamesString(configData.getModuleNames()));
    myErlangFunctionsField.setText(getCommaSeparatedNamesString(configData.getFunctionNames()));
  }

  @Override
  protected void applyEditorTo(ErlangUnitRunConfiguration configuration) throws ConfigurationException {
    configuration.setModule(myModuleComboBox.getSelectedModule());

    ErlangUnitRunConfiguration.ErlangUnitConfigData configData = configuration.getConfigData();
    configData.setFunctionNames(parseCommaSeparatedNames(myErlangFunctionsField.getText()));
    configData.setModuleNames(parseCommaSeparatedNames(myErlangModulesField.getText()));
    configData.setKind((ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind) myTestKindComboBox.getSelectedItem());
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

  private void onTestKindSwitch() {
    ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind selectedKind = (ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind) myTestKindComboBox.getSelectedItem();
    if (selectedKind == null) {
      selectedKind = ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE;
    }

    boolean functionTestSelected = selectedKind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION;
    boolean moduleTestSelected = selectedKind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE;

    myErlangFunctionsLabel.setVisible(functionTestSelected);
    myErlangFunctionsField.setVisible(functionTestSelected);
    myErlangModulesLabel.setVisible(moduleTestSelected);
    myErlangModulesField.setVisible(moduleTestSelected);
  }

  private static ListCellRendererWrapper<ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind> getTestKindListCellRendererWrapper() {
    return new ListCellRendererWrapper<ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind>() {
      @Override
      public void customize(JList list, ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind kind, int index, boolean selected, boolean hasFocus) {
        if (kind != null) {
          String kindName = StringUtil.capitalize(kind.toString().toLowerCase());
          setText(kindName);
        }
      }
    };
  }

  private static Set<String> parseCommaSeparatedNames(String names) {
    if (names == null) return Collections.emptySet();

    List<String> split = StringUtil.split(names, ",", true, true);
    Set<String> result = new LinkedHashSet<String>(split.size());

    for (String name : split) {
      result.add(name.trim());
    }
    return result;
  }

  private static String getCommaSeparatedNamesString(Set<String> names) {
    return StringUtil.join(names, ", ");
  }
}
