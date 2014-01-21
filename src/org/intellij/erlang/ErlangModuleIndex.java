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

package org.intellij.erlang;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.indexing.*;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import gnu.trove.THashSet;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class ErlangModuleIndex extends ScalarIndexExtension<String> {
  public static final ID<String, Void> ERLANG_MODULE_INDEX = ID.create("ErlangModuleIndex");
  private static final int INDEX_VERSION = 0;
  public static final FileBasedIndex.InputFilter ERLANG_MODULE_FILTER = new FileBasedIndex.InputFilter() {
    @Override
    public boolean acceptInput(VirtualFile file) {
      return file.getFileType() == ErlangFileType.MODULE;
    }
  };

  private DataIndexer<String, Void, FileContent> myDataIndexer = new MyDataIndexer();

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

  @Override
  public KeyDescriptor<String> getKeyDescriptor() {
    return new EnumeratorStringDescriptor();
  }

  @Override
  public FileBasedIndex.InputFilter getInputFilter() {
    return ERLANG_MODULE_FILTER;
  }

  @Override
  public boolean dependsOnFileContent() {
    return false;
  }

  public static Collection<String> getNames(@NotNull Project project) {
    return FileBasedIndex.getInstance().getAllKeys(ERLANG_MODULE_INDEX, project);
  }

  @NotNull
  public static List<ErlangModule> getModulesByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope) {
    final Collection<VirtualFile> files = FileBasedIndex.getInstance().getContainingFiles(ERLANG_MODULE_INDEX, name, searchScope);

    final Set<ErlangModule> result = new THashSet<ErlangModule>();
    for (VirtualFile vFile : files) {
      final PsiFile psiFile = PsiManager.getInstance(project).findFile(vFile);
      if (psiFile instanceof ErlangFile) {
        ContainerUtil.addIfNotNull(result, ((ErlangFile) psiFile).getModule());
      }
    }
    return new ArrayList<ErlangModule>(result);
  }
  
  @NotNull
  public static List<ErlangFile> getFilesByName(@NotNull Project project, @NotNull String name, @NotNull GlobalSearchScope searchScope) {
    final Collection<VirtualFile> files = FileBasedIndex.getInstance().getContainingFiles(ERLANG_MODULE_INDEX, name, searchScope);

    final Set<ErlangFile> result = new THashSet<ErlangFile>();
    for (VirtualFile vFile : files) {
      final PsiFile psiFile = PsiManager.getInstance(project).findFile(vFile);
      if (psiFile instanceof ErlangFile) {
        result.add(((ErlangFile) psiFile));
      }
    }
    return new ArrayList<ErlangFile>(result);
  }

  private static class MyDataIndexer implements DataIndexer<String, Void, FileContent> {
    @Override
    @NotNull
    public Map<String, Void> map(final FileContent inputData) {
      return Collections.singletonMap(inputData.getFile().getNameWithoutExtension(), null);
    }
  }
}
