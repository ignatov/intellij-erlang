/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Convertor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.HashSet;

public final class ErlangModulesUtil {
  private ErlangModulesUtil() {
  }

  @Nullable
  public static ErlangModule getErlangModule(final Project project, final String moduleName) {
    return new ReadAction<ErlangModule>() {
      @Override
      protected void run(Result<ErlangModule> result) throws Throwable {
        result.setResult(ContainerUtil.getFirstItem(ErlangModuleIndex.getModulesByName(project, moduleName, GlobalSearchScope.allScope(project))));
      }
    }.execute().getResultObject();
  }

  @Nullable
  public static ErlangFile getErlangModuleFile(Project project, String moduleName) {
    ErlangModule module = getErlangModule(project, moduleName);
    PsiFile containingFile = module != null ? module.getContainingFile() : null;
    return containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
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
