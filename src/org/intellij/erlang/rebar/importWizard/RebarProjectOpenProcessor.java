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

import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.projectImport.ProjectOpenProcessorBase;
import org.jetbrains.annotations.NotNull;

public class RebarProjectOpenProcessor extends ProjectOpenProcessorBase<RebarProjectImportBuilder> {
  @NotNull
  public String[] getSupportedExtensions() {
    return new String[]{"rebar.config"};
  }

  public boolean doQuickImport(@NotNull VirtualFile configFile, @NotNull WizardContext wizardContext) {
    VirtualFile projectRoot = configFile.getParent();
    wizardContext.setProjectName(projectRoot.getName());
    getBuilder().setProjectRoot(projectRoot);
    return true;
  }

  @NotNull
  @Override
  protected RebarProjectImportBuilder doGetBuilder() {
    return new RebarProjectImportBuilder();
  }
}