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

package org.intellij.erlang.utils;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Convertor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

public final class ErlangModulesUtil {
  private ErlangModulesUtil() {
  }

  @NotNull
  public static GlobalSearchScope getModuleWithDependenciesScope(@NotNull Module module, boolean includeTests) {
    return GlobalSearchScope
      .moduleWithDependenciesAndLibrariesScope(module, includeTests)
      .intersectWith(GlobalSearchScope.moduleWithDependenciesScope(module));
  }

  @Nullable
  public static ErlangModule getErlangModule(@NotNull final Project project,
                                             @NotNull final String moduleName,
                                             @NotNull final GlobalSearchScope scope) {
    return ApplicationManager.getApplication().runReadAction(new Computable<ErlangModule>() {
      @Nullable
      @Override
      public ErlangModule compute() {
        return doGetErlangModule(project, moduleName, scope);
      }
    });
  }

  @Nullable
  public static ErlangFile getErlangModuleFile(@NotNull final Project project,
                                               @NotNull final String moduleName,
                                               @NotNull final GlobalSearchScope scope) {
    return ApplicationManager.getApplication().runReadAction(new Computable<ErlangFile>() {
      @Nullable
      @Override
      public ErlangFile compute() {
        ErlangModule module = doGetErlangModule(project, moduleName, scope);
        PsiFile containingFile = module != null ? module.getContainingFile() : null;
        return containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
      }
    });
  }

  @Nullable
  private static ErlangModule doGetErlangModule(@NotNull Project project,
                                                @NotNull String moduleName,
                                                @NotNull GlobalSearchScope scope) {
    List<ErlangModule> modules = ErlangModuleIndex.getModulesByName(project, moduleName, scope);
    return ContainerUtil.getFirstItem(modules);
  }

  @NotNull
  public static Collection<ErlangFile> getErlangModules(@NotNull Project project) {
    HashSet<ErlangFile> erlangModules = new HashSet<ErlangFile>();
    for (Module module : ModuleManager.getInstance(project).getModules()) {
      addErlangModules(module, false, erlangModules);
    }
    return erlangModules;
  }

  @NotNull
  public static Collection<ErlangFile> getErlangModules(@NotNull Module module, boolean onlyTestModules) {
    return addErlangModules(module, onlyTestModules, new HashSet<ErlangFile>());
  }

  @NotNull
  private static Collection<ErlangFile> addErlangModules(@NotNull Module module, boolean onlyTestModules, @NotNull Collection<ErlangFile> erlangModules) {
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    ModuleFileIndex moduleFileIndex = rootManager.getFileIndex();
    Processor<VirtualFile> modulesCollector = getErlangModulesCollector(PsiManager.getInstance(module.getProject()), erlangModules);
    Convertor<VirtualFile, Boolean> sourceDirectoriesFilter = onlyTestModules ? getTestDirectoriesFilter(moduleFileIndex) : getSourceDirectoriesFilter(moduleFileIndex);

    for (VirtualFile sourceRoot : rootManager.getSourceRoots(onlyTestModules)) {
      VfsUtilCore.processFilesRecursively(sourceRoot, modulesCollector, sourceDirectoriesFilter);
    }

    return erlangModules;
  }

  @NotNull
  private static Convertor<VirtualFile, Boolean> getSourceDirectoriesFilter(@NotNull final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(@NotNull VirtualFile dir) {
        return moduleFileIndex.isInSourceContent(dir);
      }
    };
  }

  @NotNull
  private static Convertor<VirtualFile, Boolean> getTestDirectoriesFilter(@NotNull final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(@NotNull VirtualFile dir) {
        return moduleFileIndex.isInTestSourceContent(dir);
      }
    };
  }

  @Nullable
  private static Processor<VirtualFile> getErlangModulesCollector(@NotNull final PsiManager psiManager, @NotNull final Collection<ErlangFile> erlangFiles) {
    return new Processor<VirtualFile>() {
      @Override
      public boolean process(@NotNull VirtualFile virtualFile) {
        if (virtualFile.getFileType() == ErlangFileType.MODULE) {
          PsiFile psiFile = psiManager.findFile(virtualFile);
          if (psiFile instanceof ErlangFile) {
            erlangFiles.add((ErlangFile) psiFile);
          }
        }
        return true;
      }
    };
  }
}
