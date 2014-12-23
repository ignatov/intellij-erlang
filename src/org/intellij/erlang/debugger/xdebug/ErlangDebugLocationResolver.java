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

package org.intellij.erlang.debugger.xdebug;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangDebugLocationResolver {
  private final Project myProject;
  private final GlobalSearchScope mySessionSearchScope;
  private final PsiManager myPsiManager;

  public ErlangDebugLocationResolver(@NotNull Project project, @Nullable Module module, boolean includeTests) {
    myProject = project;
    myPsiManager = PsiManager.getInstance(myProject);
    mySessionSearchScope = module != null ?
      ErlangModulesUtil.getModuleWithDependenciesScope(module, includeTests) :
      GlobalSearchScope.projectScope(myProject);
  }

  @Nullable
  public ErlangFile resolveModule(@Nullable String moduleName) {
    return findPsi(resolveModuleFile(moduleName));
  }

  @Nullable
  public VirtualFile resolveModuleFile(@Nullable String moduleName) {
    List<VirtualFile> virtualFiles = moduleName == null ? ContainerUtil.<VirtualFile>emptyList() :
      ErlangModuleIndex.getVirtualFilesByName(myProject, moduleName, mySessionSearchScope);
    return ContainerUtil.getFirstItem(virtualFiles);
  }

  @Nullable
  public ErlangFile findPsi(@Nullable VirtualFile moduleFile) {
    PsiFile module = moduleFile != null ? myPsiManager.findFile(moduleFile) : null;
    return ObjectUtils.tryCast(module, ErlangFile.class);
  }
}
