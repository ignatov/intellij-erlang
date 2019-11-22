/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.SimpleListCellRenderer;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.utils.ErlangUiUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public final class ErlangConsoleRunConfigurationForm extends SettingsEditor<ErlangConsoleRunConfiguration> {
  private JPanel myPanel;
  private RawCommandLineEditor myConsoleArgsEditor;
  private TextFieldWithBrowseButton myWorkingDirPathField;
  private JComboBox<Module> myModuleComboBox;

  @Nullable private final Module myInitialModule;

  public ErlangConsoleRunConfigurationForm(@NotNull Project project, @Nullable Module module) {
    myInitialModule = module;
    myModuleComboBox.setEnabled(true);
    ErlangUiUtil.installWorkingDirectoryChooser(myWorkingDirPathField, project);
    myWorkingDirPathField.setText(project.getBasePath());
  }

  @Override
  protected void resetEditorFrom(@NotNull ErlangConsoleRunConfiguration config) {
    myModuleComboBox.removeAllItems();
    for (Module module : config.getValidModules()) {
      if (ModuleType.get(module) == ErlangModuleType.getInstance()) {
        myModuleComboBox.addItem(module);
      }
    }
    myModuleComboBox.setSelectedItem(myInitialModule);
    myModuleComboBox.setRenderer(getListCellRendererWrapper());

    myWorkingDirPathField.setText(config.getWorkingDirPath());
    myModuleComboBox.setSelectedItem(config.getConfigurationModule().getModule());
    myConsoleArgsEditor.setText(config.getConsoleArgs());
  }

  @Override
  protected void applyEditorTo(@NotNull ErlangConsoleRunConfiguration config) {
    config.setModule((Module) myModuleComboBox.getSelectedItem());
    config.setWorkingDirPath(myWorkingDirPathField.getText());
    config.setConsoleArgs(myConsoleArgsEditor.getText());
  }

  @NotNull
  @Override
  protected JComponent createEditor() {
    return myPanel;
  }

  @Override
  protected void disposeEditor() {
    myPanel.setVisible(false);
  }

  @NotNull
  private static SimpleListCellRenderer<Module> getListCellRendererWrapper() {
    return new SimpleListCellRenderer<Module>() {
      @Override
      public void customize(@NotNull JList<? extends Module> list, @Nullable Module module, int index, boolean selected, boolean hasFocus) {
        if (module != null) {
          setText(module.getName());
        }
      }
    };
  }
}
