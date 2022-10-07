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

package org.intellij.erlang.editor;

import com.intellij.ide.util.importProject.ModuleDescriptor;
import com.intellij.ide.util.importProject.ProjectDescriptor;
import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.ProjectJdkForModuleStep;
import com.intellij.ide.util.projectWizard.importSources.DetectedProjectRoot;
import com.intellij.ide.util.projectWizard.importSources.ProjectFromSourcesBuilder;
import com.intellij.ide.util.projectWizard.importSources.ProjectStructureDetector;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

public class ErlangProjectStructureDetector extends ProjectStructureDetector {
  @NotNull
  @Override
  public DirectoryProcessingResult detectRoots(@NotNull File dir, @NotNull File @NotNull [] children, @NotNull File base, @NotNull List<DetectedProjectRoot> result) {
    Pattern pattern = Pattern.compile(".*\\.[e|h]rl");
    List<File> filesByMask = FileUtil.findFilesByMask(pattern, base);
    if (!filesByMask.isEmpty()) {
      result.add(new DetectedProjectRoot(dir) {
        @NotNull
        @Override
        public String getRootTypeName() {
          return "Erlang";
        }
      });
    }
    return DirectoryProcessingResult.SKIP_CHILDREN;
  }

  @Override
  public void setupProjectStructure(@NotNull Collection<DetectedProjectRoot> roots, @NotNull ProjectDescriptor projectDescriptor, @NotNull ProjectFromSourcesBuilder builder) {
    if (!roots.isEmpty() && !builder.hasRootsFromOtherDetectors(this)) {
      List<ModuleDescriptor> modules = projectDescriptor.getModules();
      if (modules.isEmpty()) {
        modules = new ArrayList<>();
        for (DetectedProjectRoot root : roots) {
          modules.add(new ModuleDescriptor(root.getDirectory(), ErlangModuleType.getInstance(), ContainerUtil.emptyList()));
        }
        projectDescriptor.setModules(modules);
      }
    }
  }

  @Override
  public List<ModuleWizardStep> createWizardSteps(ProjectFromSourcesBuilder builder, ProjectDescriptor projectDescriptor, Icon stepIcon) {
    ProjectJdkForModuleStep projectJdkForModuleStep = new ProjectJdkForModuleStep(builder.getContext(), ErlangSdkType.getInstance());
    return Collections.singletonList(projectJdkForModuleStep);
  }
}
