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

import com.intellij.codeInspection.*;
import com.intellij.codeInspection.ex.DisableInspectionToolAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangModuleType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class ErlangIncludeDirectoriesInspection extends LocalInspectionTool {
  @Nullable
  @Override
  public ProblemDescriptor[] checkFile(@NotNull PsiFile psiFile, @NotNull InspectionManager manager, boolean isOnTheFly) {
    if (!(psiFile instanceof ErlangFile)) return ProblemDescriptor.EMPTY_ARRAY;
    VirtualFile file = psiFile.getVirtualFile();
    Module module = file != null ? ModuleUtilCore.findModuleForFile(file, psiFile.getProject()) : null;
    if (module == null) return ProblemDescriptor.EMPTY_ARRAY;
    List<VirtualFile> notIncludedPaths = getIncludeFoldersNotMarkedAsIncludeDirectories(module);
    if (!notIncludedPaths.isEmpty()) {
      return createProblemDescriptors(psiFile, "Some 'include' folders are not marked as include directories.", manager, isOnTheFly);
    }
    return ProblemDescriptor.EMPTY_ARRAY;
  }

  private static ProblemDescriptor[] createProblemDescriptors(PsiFile psiFile, String message, InspectionManager manager, boolean isOnTheFly) {
    return new ProblemDescriptor[]{
      manager.createProblemDescriptor(
        psiFile, message, new LocalQuickFix[]{new ErlangIncludeDirectoriesQuickFix(true), new ErlangIncludeDirectoriesQuickFix(false)},
        ProblemHighlightType.GENERIC_ERROR_OR_WARNING, isOnTheFly, false)
    };
  }

  private static List<VirtualFile> getIncludeFoldersNotMarkedAsIncludeDirectories(Module module) {
    final List<VirtualFile> includeDirectories = ErlangIncludeDirectoryUtil.getIncludeDirectories(module);
    List<VirtualFile> includeFolders = getIncludeFolders(module);
    return ContainerUtil.filter(includeFolders, new Condition<VirtualFile>() {
      @Override
      public boolean value(VirtualFile includeFolderPath) {
        return !includeDirectories.contains(includeFolderPath);
      }
    });
  }

  private static List<VirtualFile> getIncludeFolders(Module module) {
    List<VirtualFile> includeFolders = new ArrayList<VirtualFile>();
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
