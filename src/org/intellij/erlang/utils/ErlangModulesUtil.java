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

  @Nullable
  public static ErlangModule getErlangModule(final Project project, final String moduleName) {
    return ApplicationManager.getApplication().runReadAction(new Computable<ErlangModule>() {
      @Nullable
      @Override
      public ErlangModule compute() {
        return doGetErlangModule(project, moduleName);
      }
    });
  }

  @Nullable
  public static ErlangFile getErlangModuleFile(final Project project, final String moduleName) {
    return ApplicationManager.getApplication().runReadAction(new Computable<ErlangFile>() {
      @Nullable
      @Override
      public ErlangFile compute() {
        ErlangModule module = doGetErlangModule(project, moduleName);
        PsiFile containingFile = module != null ? module.getContainingFile() : null;
        return containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
      }
    });
  }

  @Nullable
  private static ErlangModule doGetErlangModule(@NotNull Project project, @NotNull String moduleName) {
    List<ErlangModule> modules = ErlangModuleIndex.getModulesByName(project, moduleName, GlobalSearchScope.allScope(project));
    return ContainerUtil.getFirstItem(modules);
  }

  public static Collection<ErlangFile> getErlangModules(Project project) {
    HashSet<ErlangFile> erlangModules = new HashSet<ErlangFile>();
    for (Module module : ModuleManager.getInstance(project).getModules()) {
      addErlangModules(module, false, erlangModules);
    }
    return erlangModules;
  }

  public static Collection<ErlangFile> getErlangModules(Module module, boolean onlyTestModules) {
    return addErlangModules(module, onlyTestModules, new HashSet<ErlangFile>());
  }

  private static Collection<ErlangFile> addErlangModules(Module module, boolean onlyTestModules, Collection<ErlangFile> erlangModules) {
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    ModuleFileIndex moduleFileIndex = rootManager.getFileIndex();
    Processor<VirtualFile> modulesCollector = getErlangModulesCollector(PsiManager.getInstance(module.getProject()), erlangModules);
    Convertor<VirtualFile, Boolean> sourceDirectoriesFilter = onlyTestModules ? getTestDirectoriesFilter(moduleFileIndex) : getSourceDirectoriesFilter(moduleFileIndex);

    for (VirtualFile sourceRoot : rootManager.getSourceRoots(onlyTestModules)) {
      VfsUtilCore.processFilesRecursively(sourceRoot, modulesCollector, sourceDirectoriesFilter);
    }

    return erlangModules;
  }

  private static Convertor<VirtualFile, Boolean> getSourceDirectoriesFilter(final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(VirtualFile dir) {
        return moduleFileIndex.isInSourceContent(dir);
      }
    };
  }

  private static Convertor<VirtualFile, Boolean> getTestDirectoriesFilter(final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(VirtualFile dir) {
        return moduleFileIndex.isInTestSourceContent(dir);
      }
    };
  }

  private static Processor<VirtualFile> getErlangModulesCollector(final PsiManager psiManager, final Collection<ErlangFile> erlangFiles) {
    return new Processor<VirtualFile>() {
      @Override
      public boolean process(VirtualFile virtualFile) {
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
