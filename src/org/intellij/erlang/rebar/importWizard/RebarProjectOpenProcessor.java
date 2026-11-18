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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.projectImport.ProjectOpenProcessorBase;
import org.intellij.erlang.rebar.runner.RebarRunningStateUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
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
    builder.setList(selectApps(projectRoot, builder.getList()));
    return true;
  }

  @NotNull
  protected String findRebarPath(@NotNull VirtualFile projectRoot) {
    return RebarRunningStateUtil.getRebarPath(projectRoot.getPath());
  }

  // Without the wizard there is no SelectImportedOtpAppsStep to deselect apps. Skip the apps of releases under
  // _build/<profile>/rel: they are OTP's own apps or links to them. Of apps with the same name, such as an app and its
  // copy in _build/default/lib after rebar3 compile, keep the one outside _build, then the one fewest directories deep.
  @NotNull
  private static List<ImportedOtpApp> selectApps(@NotNull VirtualFile projectRoot, @NotNull List<ImportedOtpApp> apps) {
    Map<String, ImportedOtpApp> appsByName = new LinkedHashMap<>();
    Map<String, Integer> ranksByName = new HashMap<>();
    for (ImportedOtpApp app : apps) {
      String relativePath = VfsUtilCore.getRelativePath(app.getRoot(), projectRoot);
      List<String> segments = relativePath == null || relativePath.isEmpty() ? List.of() : StringUtil.split(relativePath, "/");
      boolean inBuild = !segments.isEmpty() && "_build".equals(segments.get(0));
      if (inBuild && segments.size() > 2 && "rel".equals(segments.get(2))) continue;
      int rank = (inBuild ? 1000 : 0) + segments.size();
      Integer otherRank = ranksByName.get(app.getName());
      if (otherRank == null || rank < otherRank) {
        appsByName.put(app.getName(), app);
        ranksByName.put(app.getName(), rank);
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