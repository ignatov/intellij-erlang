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

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.codeInspection.ex.DisableInspectionToolAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangIncludeDirectoriesInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder problemsHolder) {
    VirtualFile virtualFile = file.getVirtualFile();
    Module module = virtualFile != null ? ModuleUtilCore.findModuleForFile(virtualFile, file.getProject()) : null;
    if (module != null) {
      List<VirtualFile> notIncludedPaths = getIncludeFoldersNotMarkedAsIncludeDirectories(module);
      if (!notIncludedPaths.isEmpty()) {
        problemsHolder.registerProblem(file,
          "Some 'include' folders are not marked as include directories.", ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
          new ErlangIncludeDirectoriesQuickFix(true), new ErlangIncludeDirectoriesQuickFix(false));
      }
    }
  }

  private static List<VirtualFile> getIncludeFoldersNotMarkedAsIncludeDirectories(Module module) {
    final List<VirtualFile> includeDirectories = ErlangIncludeDirectoryUtil.getIncludeDirectories(module);
    List<VirtualFile> includeFolders = getIncludeFolders(module);
    return ContainerUtil.filter(includeFolders, includeFolderPath -> !includeDirectories.contains(includeFolderPath));
  }

  private static List<VirtualFile> getIncludeFolders(Module module) {
    List<VirtualFile> includeFolders = new ArrayList<>();
    VirtualFile[] contentRoots = ModuleRootManager.getInstance(module).getContentRoots();
    for (VirtualFile contentRoot : contentRoots) {
      VirtualFile includeDirectory = VfsUtil.findRelativeFile(contentRoot, "include");
      if (includeDirectory != null) {
        includeFolders.add(includeDirectory);
      }
    }
    return includeFolders;
  }

  private static class ErlangIncludeDirectoriesQuickFix implements LocalQuickFix {
    private boolean myDoFix;
    private final String myName;

    protected ErlangIncludeDirectoriesQuickFix(boolean doFix) {
      myName = doFix ? "Mark folders" : "Dismiss";
      myDoFix = doFix;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      if (myDoFix) {
        for (Module module : ModuleManager.getInstance(project).getModules()) {
          doFix(module);
        }
      }
      else {
        new DisableInspectionToolAction(new ErlangIncludeDirectoriesInspection()).invoke(project, null, descriptor.getPsiElement().getContainingFile());
      }
    }

    @NotNull
    @Override
    public String getName() {
      return myName;
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return myName;
    }

    private static void doFix(@NotNull Module module) {
      if (ModuleType.get(module) != ErlangModuleType.getInstance()) return;
      List<VirtualFile> includeFolders = getIncludeFoldersNotMarkedAsIncludeDirectories(module);
      for (VirtualFile includeFolder : includeFolders) {
        ErlangIncludeDirectoryUtil.markAsIncludeDirectory(module, includeFolder);
      }
    }
  }
}
