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
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.projectImport.ProjectOpenProcessorBase;
import org.intellij.erlang.rebar.runner.RebarRunningStateUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class RebarProjectOpenProcessor extends ProjectOpenProcessorBase<RebarProjectImportBuilder> {
  private final RebarProjectImportBuilder myBuilder = new RebarProjectImportBuilder();

  @NotNull
  public String[] getSupportedExtensions() {
    return new String[]{"rebar.config"};
  }

  public boolean doQuickImport(@NotNull VirtualFile configFile, @NotNull WizardContext wizardContext) {
    VirtualFile projectRoot = configFile.getParent();
    wizardContext.setProjectName(projectRoot.getName());
    String rebarPath = ProgressManager.getInstance().runProcessWithProgressSynchronously(
      () -> findRebarPath(projectRoot), "Detecting Rebar", false, wizardContext.getProject());
    RebarProjectImportBuilder builder = getBuilder();
    builder.setRebarPath(rebarPath);
    builder.setIsImportingProject(wizardContext.isCreatingNewProject() && !rebarPath.isEmpty());
    builder.setProjectRoot(projectRoot);
    builder.setList(selectOneAppPerName(builder.getList()));
    return true;
  }

  @NotNull
  protected String findRebarPath(@NotNull VirtualFile projectRoot) {
    return RebarRunningStateUtil.getRebarPath(projectRoot.getPath());
  }

  // Without the wizard there is no SelectImportedOtpAppsStep to resolve name conflicts, for example an app and its copy
  // in _build/default/lib after rebar3 compile. Keep the app whose root is nearest to the project root.
  @NotNull
  private static List<ImportedOtpApp> selectOneAppPerName(@NotNull List<ImportedOtpApp> apps) {
    Map<String, ImportedOtpApp> appsByName = new LinkedHashMap<>();
    for (ImportedOtpApp app : apps) {
      ImportedOtpApp other = appsByName.get(app.getName());
      if (other == null || app.getRoot().getPath().length() < other.getRoot().getPath().length()) {
        appsByName.put(app.getName(), app);
      }
    }
    return new ArrayList<>(appsByName.values());
  }

  @NotNull
  @Override
  protected RebarProjectImportBuilder doGetBuilder() {
    return myBuilder;
  }
}