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

package org.intellij.erlang.debugger.remote.ui;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.ui.ListCellRendererWrapper;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.editor.ErlangModuleType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ErlangRemoteDebugConfigurationEditorForm extends SettingsEditor<ErlangRemoteDebugRunConfiguration> {
  private JPanel myComponent;
  private JComboBox myModuleComboBox;
  private JTextField myNodeTextField;
  private JTextField myCookieTextField;

  @SuppressWarnings("unchecked")
  @Override
  protected void resetEditorFrom(ErlangRemoteDebugRunConfiguration configuration) {
    myModuleComboBox.removeAllItems();
    final Module[] modules = ModuleManager.getInstance(configuration.getProject()).getModules();
    for (final Module module : modules) {
      if (ModuleType.get(module) == ErlangModuleType.getInstance()) {
        myModuleComboBox.addItem(module);
      }
    }
    myModuleComboBox.setSelectedItem(configuration.getConfigurationModule().getModule());
    myModuleComboBox.setRenderer(getListCellRendererWrapper());
    myNodeTextField.setText(configuration.getErlangNode());
    myCookieTextField.setText(configuration.getCookie());
  }

  @Override
  protected void applyEditorTo(ErlangRemoteDebugRunConfiguration configuration) throws ConfigurationException {
    configuration.setModule((Module) myModuleComboBox.getSelectedItem());
    configuration.setErlangNode(myNodeTextField.getText());
    configuration.setCookie(myCookieTextField.getText());
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return myComponent;
  }

  private static ListCellRendererWrapper getListCellRendererWrapper() {
    return new ListCellRendererWrapper() {
      @Override
      public void customize(JList list, Object value, int index, boolean selected, boolean hasFocus) {
        if (value instanceof Module) {
          final Module module = (Module) value;
          setText(module.getName());
        }
      }
    };
  }
}
