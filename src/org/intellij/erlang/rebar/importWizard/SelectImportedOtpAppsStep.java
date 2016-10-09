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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.ide.util.ElementsChooser;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.projectImport.SelectImportedProjectsStep;
import org.intellij.erlang.icons.ErlangIcons;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

final class SelectImportedOtpAppsStep extends SelectImportedProjectsStep<ImportedOtpApp> {
  private final Set<String> myDuplicateModuleNames = new HashSet<String>();

  public SelectImportedOtpAppsStep(@NotNull WizardContext context) {
    super(context);
    fileChooser.addElementsMarkListener((ElementsChooser.ElementsMarkListener<ImportedOtpApp>) (importedOtpApp, isMarked) -> {
      evalDuplicates();
      fileChooser.repaint();
    });
  }

  @Override
  public void updateStep() {
    super.updateStep();
    evalDuplicates();
  }

  @Override
  public boolean validate() throws ConfigurationException {
    return super.validate() && myDuplicateModuleNames.isEmpty();
  }

  public void autoResolveConflicts() {
    // NOTE: It is assumed that elements are sorted by names, therefore conflicting names a grouped together.
    String previousAppName = null;
    for (ImportedOtpApp selectedOtpApp : fileChooser.getMarkedElements()) {
      if (selectedOtpApp.getName().equals(previousAppName)) {
        fileChooser.setElementMarked(selectedOtpApp, false);
      }
      else {
        previousAppName = selectedOtpApp.getName();
      }
    }
  }

  @Override
  protected String getElementText(@NotNull ImportedOtpApp importedOtpApp) {
    return importedOtpApp.toString();
  }

  @Nullable
  @Override
  protected Icon getElementIcon(@NotNull ImportedOtpApp importedOtpApp) {
    return myDuplicateModuleNames.contains(importedOtpApp.getName())
      ? ErlangIcons.REBAR_MODULE_CONFLICT : null;
  }

  private void evalDuplicates() {
    List<ImportedOtpApp> selectedOtpApps = fileChooser.getMarkedElements();
    Set<String> contains = new HashSet<String>(selectedOtpApps.size());
    myDuplicateModuleNames.clear();
    for (ImportedOtpApp importedOtpApp : selectedOtpApps) {
      if (!contains.add(importedOtpApp.getName())) {
        myDuplicateModuleNames.add(importedOtpApp.getName());
      }
    }
  }
}
