/*
 * Copyright 2012-2019 Sergey Ignatov
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

package org.intellij.erlang.psi.impl;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectAndLibrariesScope;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.index.ErlangApplicationIndex;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Comparator;
import java.util.List;

public class ErlangModuleReferenceImpl extends ErlangQAtomBasedReferenceImpl {
  private static final int COMPARE_NO_RESULT = Integer.MIN_VALUE;

  public ErlangModuleReferenceImpl(@NotNull PsiElement owner, @NotNull ErlangQAtom qAtom) {
    super(owner, qAtom, ErlangPsiImplUtil.getTextRangeForReference(owner, qAtom), ErlangPsiImplUtil.getNameIdentifier(qAtom).getText());
  }

  @Override
  public PsiElement resolveInner() {
    GlobalSearchScope scope = getSearchScope();
    List<ErlangModule> modules = ErlangModuleIndex.getModulesByName(myElement.getProject(), myReferenceName, scope);
    if (modules.size() > 1) {
      ContainerUtil.sort(modules, new ModuleResolutionComparator());
    }
    return ContainerUtil.getFirstItem(modules);
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    return ArrayUtil.EMPTY_OBJECT_ARRAY;
  }

  @NotNull
  private GlobalSearchScope getSearchScope() {
    Project project = myElement.getProject();
    Module module = ModuleUtilCore.findModuleForPsiElement(myElement);
    return module != null ? module.getModuleWithDependenciesAndLibrariesScope(true) :
           new ProjectAndLibrariesScope(project);
  }

  private final class ModuleResolutionComparator implements Comparator<ErlangModule> {
    private final VirtualFile myStdLibDir;
    private final VirtualFile myKernelLibDir;
    private final ProjectFileIndex myFileIndex;

    public ModuleResolutionComparator() {
      GlobalSearchScope scope = getSearchScope();
      myStdLibDir = ErlangApplicationIndex.getApplicationDirectoryByName("stdlib", scope);
      myKernelLibDir = ErlangApplicationIndex.getApplicationDirectoryByName("kernel", scope);
      myFileIndex = ProjectRootManager.getInstance(myElement.getProject()).getFileIndex();
    }

    @Override
    public int compare(ErlangModule m1, ErlangModule m2) {
      VirtualFile m1File = getVirtualFile(m1);
      VirtualFile m2File = getVirtualFile(m2);
      if (m1File == null) {
        return m2File == null ? 0 : 1;
      }
      if (m2File == null) {
        return -1;
      }

      // prefer modules from stdlib and kernel (see "Code Path" at http://www.erlang.org/doc/man/code.html)
      int byStdLibDir = compareByLibDir(m1File, m2File, myStdLibDir);
      if (byStdLibDir != COMPARE_NO_RESULT) {
        return byStdLibDir;
      }
      int byKernelLibDir = compareByLibDir(m1File, m2File, myKernelLibDir);
      if (byKernelLibDir != COMPARE_NO_RESULT) {
        return byKernelLibDir;
      }

      // prefer user modules to SDK modules (see "Code Path" at http://www.erlang.org/doc/man/code.html)
      boolean m1IsInLib = myFileIndex.isInLibrarySource(m1File);
      boolean m2IsInLib = myFileIndex.isInLibrarySource(m2File);
      int byBelongingToLib = (m1IsInLib ? 1 : 0) + (m2IsInLib ? -1 : 0);
      if (byBelongingToLib != 0) {
        return byBelongingToLib;
      }

      // fallback to comparison by path. Ideally, we should maintain code path and sort according to it.
      return FileUtil.comparePaths(m1File.getPath(), m2File.getPath());
    }

    private static int compareByLibDir(@NotNull VirtualFile m1File,
                                       @NotNull VirtualFile m2File,
                                       @Nullable VirtualFile libDir) {
      if (libDir == null) return COMPARE_NO_RESULT;

      boolean m1InLib = VfsUtilCore.isAncestor(libDir, m1File, true);
      boolean m2InLib = VfsUtilCore.isAncestor(libDir, m2File, true);
      if (m1InLib || m2InLib) {
        return (m1InLib ? -1 : 0) + (m2InLib ? 1 : 0);
      }

      return COMPARE_NO_RESULT;
    }

    @Nullable
    private static VirtualFile getVirtualFile(@NotNull PsiElement e) {
      PsiFile psiFile = e.getContainingFile();
      return psiFile != null ? psiFile.getVirtualFile() : null;
    }
  }
}
