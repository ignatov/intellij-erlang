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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.projectImport.ProjectImportWizardStep;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

final class RebarProjectRootStep extends ProjectImportWizardStep {
  private JPanel myPanel;
  private TextFieldWithBrowseButton myProjectRootComponent;
  private JCheckBox myGetDepsCheckbox;
  private JCheckBox myImportExamplesCheckBox;

  public RebarProjectRootStep(final WizardContext context) {
    super(context);
    myProjectRootComponent.addBrowseFolderListener("Select rebar.config of the project to import", "", null,
      FileChooserDescriptorFactory.createSingleFolderDescriptor());
    String projectFileDirectory = context.getProjectFileDirectory();
    myProjectRootComponent.setText(projectFileDirectory); // provide project path

    String rebarExecutable = RebarProjectImportBuilder.getRebarExecutable(projectFileDirectory);
    boolean rebarExists = !StringUtil.isEmptyOrSpaces(rebarExecutable);
    myGetDepsCheckbox.setEnabled(rebarExists);
    myGetDepsCheckbox.setVisible(!SystemInfo.isWindows);
    myGetDepsCheckbox.setToolTipText(rebarExists ? "Fetch dependencies via '" + rebarExecutable.trim() + " get-deps'" : "Can not find rebar executable in the PATH");
  }

  @Override
  public boolean validate() throws ConfigurationException {
    final String projectRootPath = myProjectRootComponent.getText();
    if (StringUtil.isEmpty(projectRootPath)) {
      return false;
    }
    final VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByPath(projectRootPath);
    if (projectRoot == null) return false;
    if (myGetDepsCheckbox.isSelected() && myGetDepsCheckbox.isEnabled() && !ApplicationManager.getApplication().isUnitTestMode()) {
      RebarProjectImportBuilder.fetchDependencies(projectRoot);
    }
    RebarProjectImportBuilder builder = getBuilder();
    builder.setImportExamples(myImportExamplesCheckBox.isSelected());
    return builder.setProjectRoot(projectRoot);
  }

  @Override
  @NotNull
  public JComponent getComponent() {
    return myPanel;
  }

  @Override
  public void updateDataModel() {
    final String projectRoot = myProjectRootComponent.getText();
    if (!projectRoot.isEmpty()) {
      suggestProjectNameAndPath(null, projectRoot);
    }
  }

  @Override
  @NotNull
  public JComponent getPreferredFocusedComponent() {
    return myProjectRootComponent.getTextField();
  }

  @Override
  @NotNull
  protected RebarProjectImportBuilder getBuilder() {
    return (RebarProjectImportBuilder) super.getBuilder();
  }
}
