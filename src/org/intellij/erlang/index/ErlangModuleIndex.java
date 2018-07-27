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

package org.intellij.erlang.index;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.indexing.*;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.*;

public class ErlangModuleIndex extends ScalarIndexExtension<String> {
  private static final ID<String, Void> ERLANG_MODULE_INDEX = ID.create("ErlangModuleIndex");
  private static final int INDEX_VERSION = 1;
  private static final EnumeratorStringDescriptor DESCRIPTOR = new EnumeratorStringDescriptor();

  @NotNull
  private final DataIndexer<String, Void, FileContent> myDataIndexer = new MyDataIndexer();

  @NotNull
  @Override
  public ID<String, Void> getName() {
    return ERLANG_MODULE_INDEX;
  }

  @Override
  public int getVersion() {
    return INDEX_VERSION;
  }

  @NotNull
  @Override
  public DataIndexer<String, Void, FileContent> getIndexer() {
    return myDataIndexer;
  }

  @NotNull
  @Override
  public KeyDescriptor<String> getKeyDescriptor() {
    return DESCRIPTOR;
  }

  @NotNull
  @Override
  public FileBasedIndex.InputFilter getInputFilter() {
    return ErlangIndexUtil.ERLANG_MODULE_FILTER;
  }

  @Override
  public boolean dependsOnFileContent() {
    return false;
  }

  @NotNull
  public static Collection<String> getNames(@NotNull Project project) {
    return FileBasedIndex.getInstance().getAllKeys(ERLANG_MODULE_INDEX, project);
  }

  @NotNull
  public static List<ErlangModule> getModulesByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope) {
    return getByName(project, name, searchScope, ErlangFile::getModule);
  }

  @NotNull
  public static List<ErlangFile> getFilesByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope) {
    return getByName(project, name, searchScope, erlangFile -> erlangFile);
  }

  @NotNull
  private static <T> List<T> getByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope, @NotNull final Function<ErlangFile, T> psiMapper) {
    final PsiManager psiManager = PsiManager.getInstance(project);
    List<VirtualFile> virtualFiles = getVirtualFilesByName(project, name, searchScope);
    return ContainerUtil.mapNotNull(virtualFiles, virtualFile -> {
      PsiFile psiFile = psiManager.findFile(virtualFile);
      return psiFile instanceof ErlangFile ? psiMapper.fun((ErlangFile)psiFile) : null;
    });
  }

  @NotNull
  public static List<VirtualFile> getVirtualFilesByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope) {
    ProjectFileIndex projectFileIndex = ProjectRootManager.getInstance(project).getFileIndex();
    Collection<VirtualFile> files = FileBasedIndex.getInstance().getContainingFiles(ERLANG_MODULE_INDEX, name, searchScope);
    List<VirtualFile> filesList = ContainerUtil.newArrayList(files);
    filesList.sort(new MyProjectFilesComparator(projectFileIndex, searchScope));
    return filesList;
  }

  private static final class MyProjectFilesComparator implements Comparator<VirtualFile> {
    private final ProjectFileIndex myProjectFileIndex;
    private final GlobalSearchScope mySearchScope;

    MyProjectFilesComparator(ProjectFileIndex pfi, GlobalSearchScope searchScope) {
      myProjectFileIndex = pfi;
      mySearchScope = searchScope;
    }

    @Override
    public int compare(@NotNull VirtualFile f1, @NotNull VirtualFile f2) {
      // according to http://www.erlang.org/doc/man/code.html, modules that belong to
      // 'kernel' and 'stdlib' applications always appear before any user-defined modules
      if (isKernelOrStdlibModule(f1)) return -1;
      if (isKernelOrStdlibModule(f2)) return 1;

      boolean f1IsInSource = isInSource(f1);
      boolean f2IsInSource = isInSource(f2);
      if (f1IsInSource != f2IsInSource) return f1IsInSource ? -1 : 1;

      boolean f1IsHidden = isUnderHiddenDirectory(f1);
      boolean f2IsHidden = isUnderHiddenDirectory(f2);
      if (f1IsHidden != f2IsHidden) return f1IsHidden ? 1 : -1;

      boolean f1IsInLibrary = isInLibrary(f1);
      boolean f2IsInLibrary = isInLibrary(f2);
      if (f1IsInLibrary != f2IsInLibrary) return f1IsInLibrary ? -1 : 1;

      return f1.getPath().length() - f2.getPath().length();
    }

    private boolean isKernelOrStdlibModule(@NotNull VirtualFile file) {
      VirtualFile kernelAppDir = ErlangApplicationIndex.getApplicationDirectoryByName("kernel", mySearchScope);
      if (kernelAppDir != null && VfsUtilCore.isAncestor(kernelAppDir, file, true)) return true;
      VirtualFile stdlibAppDir = ErlangApplicationIndex.getApplicationDirectoryByName("stdlib", mySearchScope);
      return stdlibAppDir != null && VfsUtilCore.isAncestor(stdlibAppDir, file, true);
    }

    private boolean isUnderHiddenDirectory(VirtualFile f) {
      VirtualFile contentRoot = myProjectFileIndex.getContentRootForFile(f);
      while (f != null && (contentRoot == null || VfsUtilCore.isAncestor(contentRoot, f, true))) {
        File ioFile = VfsUtilCore.virtualToIoFile(f);
        if (ioFile.isHidden()) return true;
        f = f.getParent();
      }
      return false;
    }

    private boolean isInLibrary(@NotNull VirtualFile file) {
      return myProjectFileIndex.isInLibraryClasses(file) || myProjectFileIndex.isInLibrarySource(file);
    }

    private boolean isInSource(@NotNull VirtualFile file) {
      return myProjectFileIndex.isInSource(file);
    }
  }

  private static class MyDataIndexer implements DataIndexer<String, Void, FileContent> {
    @Override
    @NotNull
    public Map<String, Void> map(@NotNull FileContent inputData) {
      return Collections.singletonMap(inputData.getFile().getNameWithoutExtension(), null);
    }
  }
}
