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
import java.util.LinkedList;
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
    return ApplicationManager.getApplication().runReadAction((Computable<ErlangModule>) () -> doGetErlangModule(project, moduleName, scope));
  }

  @Nullable
  public static ErlangFile getErlangModuleFile(@NotNull final Project project,
                                               @NotNull final String moduleName,
                                               @NotNull final GlobalSearchScope scope) {
    return ApplicationManager.getApplication().runReadAction((Computable<ErlangFile>) () -> {
      ErlangModule module = doGetErlangModule(project, moduleName, scope);
      PsiFile containingFile = module != null ? module.getContainingFile() : null;
      return containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
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
    HashSet<ErlangFile> erlangModules = new HashSet<>();
    for (Module module : ModuleManager.getInstance(project).getModules()) {
      addErlangFiles(module, false, erlangModules, ErlangFileType.MODULE);
    }
    return erlangModules;
  }

  @NotNull
  public static Collection<ErlangFile> getErlangModules(@NotNull Module module, boolean onlyTestModules) {
    return addErlangFiles(module, onlyTestModules, new HashSet<>(), ErlangFileType.MODULE);
  }

  @NotNull
  public static Collection<VirtualFile> getErlangHeaderFiles(@NotNull Module module, boolean onlyTestModules) {
    return addFiles(module, onlyTestModules, new HashSet<>(), ErlangFileType.HEADER);
  }

  @NotNull
  public static Collection<VirtualFile> getErlangModuleFiles(@NotNull Module module, boolean onlyTestModules) {
    return addFiles(module, onlyTestModules, new HashSet<>(), ErlangFileType.MODULE);
  }

  @NotNull
  private static Collection<ErlangFile> addErlangFiles(@NotNull Module module,
                                                       boolean onlyTestModules,
                                                       @NotNull Collection<ErlangFile> erlangModules,
                                                       @NotNull ErlangFileType type) {
    Processor<VirtualFile> filesCollector = getErlangModulesCollector(PsiManager.getInstance(module.getProject()), erlangModules, type);
    collectFiles(module, onlyTestModules, filesCollector);
    return erlangModules;
  }

  @NotNull
  private static Collection<VirtualFile> addFiles(@NotNull Module module,
                                                  boolean onlyTest,
                                                  @NotNull final Collection<VirtualFile> files,
                                                  @NotNull final ErlangFileType type) {
    Processor<VirtualFile> modulesCollector = virtualFile -> {
      if (virtualFile.getFileType() == type) {
        files.add(virtualFile);
      }
      return true;
    };
    collectFiles(module, onlyTest, modulesCollector);
    return files;
  }

  private static void collectFiles(@NotNull Module module,
                                   boolean onlyTestModules,
                                   @NotNull Processor<VirtualFile> filesCollector) {
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    ModuleFileIndex moduleFileIndex = rootManager.getFileIndex();
    Convertor<VirtualFile, Boolean> sourceDirectoriesFilter = onlyTestModules ? getTestDirectoriesFilter(moduleFileIndex)
                                                                              : getSourceDirectoriesFilter(moduleFileIndex);

    for (VirtualFile sourceRoot : rootManager.getSourceRoots(onlyTestModules)) {
      processFilesRecursively(sourceRoot, filesCollector, sourceDirectoriesFilter);
    }
  }

  @Deprecated
  public static void processFilesRecursively(@NotNull VirtualFile root,
                                             @NotNull Processor<? super VirtualFile> processor,
                                             @NotNull Convertor<? super VirtualFile, Boolean> directoryFilter) {
    if (!processor.process(root)) return;

    if (root.isDirectory() && directoryFilter.convert(root)) {
      final LinkedList<VirtualFile[]> queue = new LinkedList<>();

      queue.add(root.getChildren());

      do {
        final VirtualFile[] files = queue.removeFirst();

        for (VirtualFile file : files) {
          if (!processor.process(file)) return;
          if (file.isDirectory() && directoryFilter.convert(file)) {
            queue.add(file.getChildren());
          }
        }
      } while (!queue.isEmpty());
    }
  }
  
  
  @NotNull
  private static Convertor<VirtualFile, Boolean> getSourceDirectoriesFilter(@NotNull final ModuleFileIndex moduleFileIndex) {
    return moduleFileIndex::isInSourceContent;
  }

  @NotNull
  private static Convertor<VirtualFile, Boolean> getTestDirectoriesFilter(@NotNull final ModuleFileIndex moduleFileIndex) {
    return moduleFileIndex::isInTestSourceContent;
  }

  @NotNull
  private static Processor<VirtualFile> getErlangModulesCollector(@NotNull final PsiManager psiManager,
                                                                  @NotNull final Collection<ErlangFile> erlangFiles,
                                                                  @NotNull final ErlangFileType type) {
    return virtualFile -> {
      if (virtualFile.getFileType() == type) {
        PsiFile psiFile = psiManager.findFile(virtualFile);
        if (psiFile instanceof ErlangFile) {
          erlangFiles.add((ErlangFile) psiFile);
        }
      }
      return true;
    };
  }
}
