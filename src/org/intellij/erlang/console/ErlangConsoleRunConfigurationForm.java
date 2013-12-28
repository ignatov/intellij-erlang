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

package org.intellij.erlang.console;

import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.ListCellRendererWrapper;
import com.intellij.ui.RawCommandLineEditor;
import org.intellij.erlang.ErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public final class ErlangConsoleRunConfigurationForm extends SettingsEditor<ErlangConsoleRunConfiguration> {
  private JPanel myPanel;
  private RawCommandLineEditor myConsoleArgsEditor;
  private TextFieldWithBrowseButton myWorkingDirPathField;
  private JComboBox myModuleComboBox;

  @Nullable private final Module myInitialModule;

  public ErlangConsoleRunConfigurationForm(@NotNull Project project, @Nullable Module module) {
    myInitialModule = module;
    myModuleComboBox.setEnabled(true);
    addFileChooser("Choose Working Directory", myWorkingDirPathField, project);
    myWorkingDirPathField.setText(project.getBasePath());
  }

  @Override
  protected void resetEditorFrom(@NotNull ErlangConsoleRunConfiguration config) {
    myModuleComboBox.removeAllItems();
    for (final Module module : config.getValidModules()) {
      if (ModuleType.get(module) == ErlangModuleType.getInstance()) {
        myModuleComboBox.addItem(module);
      }
    }
    myModuleComboBox.setSelectedItem(myInitialModule);
    //noinspection unchecked
    myModuleComboBox.setRenderer(getListCellRendererWrapper());

    myWorkingDirPathField.setText(config.getWorkingDirPath());
    myModuleComboBox.setSelectedItem(config.getConfigurationModule().getModule());
    myConsoleArgsEditor.setText(config.getConsoleArgs());
  }

  @Override
  protected void applyEditorTo(@NotNull ErlangConsoleRunConfiguration config) throws ConfigurationException {
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
  private static FileChooserDescriptor addFileChooser(@NotNull final String title,
                                                      @NotNull final TextFieldWithBrowseButton textField,
                                                      @NotNull final Project project) {
    final FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(
      false, true, false, false, false, false) {
      @Override
      public boolean isFileVisible(VirtualFile file, boolean showHiddenFiles) {
        return super.isFileVisible(file, showHiddenFiles) && file.isDirectory();
      }
    };
    fileChooserDescriptor.setTitle(title);
    textField.addBrowseFolderListener(title, null, project, fileChooserDescriptor);
    return fileChooserDescriptor;
  }

  @NotNull
  public static ListCellRendererWrapper<Module> getListCellRendererWrapper() {
    return new ListCellRendererWrapper<Module>() {
      @Override
      public void customize(JList list, @Nullable Module module, int index, boolean selected, boolean hasFocus) {
        if (module != null) {
          setText(module.getName());
        }
      }
    };
  }
}
