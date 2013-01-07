/*
 * Copyright 2013 Sergey Ignatov
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

import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.indexing.*;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import gnu.trove.THashMap;
import gnu.trove.THashSet;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author ignatov
 */
public class ErlangSymbolIndex extends ScalarIndexExtension<String> {
  public static final ID<String, Void> ERLANG_SYMBOL_INDEX = ID.create("ErlangSymbolIndex");
  private static final int INDEX_VERSION = 0;
  private DataIndexer<String, Void, FileContent> myDataIndexer = new MyDataIndexer();

  @NotNull
  @Override
  public ID<String, Void> getName() {
    return ERLANG_SYMBOL_INDEX;
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
    return ERLANG_INPUT_FILTER;
  }

  @Override
  public boolean dependsOnFileContent() {
    return true;
  }

  public static final FileBasedIndex.InputFilter ERLANG_INPUT_FILTER = new FileBasedIndex.InputFilter() {
    @Override
    public boolean acceptInput(VirtualFile file) {
      return isErlangFileType(file.getFileType());
    }
  };

  private static boolean isErlangFileType(FileType fileType) {
    return fileType == ErlangFileType.MODULE || fileType == ErlangFileType.HEADER;
  }

  public static Collection<String> getNames(Project project) {
    return FileBasedIndex.getInstance().getAllKeys(ERLANG_SYMBOL_INDEX, project);
  }

  public static List<ErlangNamedElement> getItemsByName(final String name, Project project, GlobalSearchScope searchScope) {
    final Collection<VirtualFile> files =
      FileBasedIndex.getInstance().getContainingFiles(ERLANG_SYMBOL_INDEX, name, searchScope);
    final Set<ErlangNamedElement> result = new THashSet<ErlangNamedElement>();
    for (VirtualFile vFile : files) {
      final PsiFile psiFile = PsiManager.getInstance(project).findFile(vFile);
      if (psiFile == null || !isErlangFileType(psiFile.getFileType())) {
        continue;
      }
      processComponents(psiFile, new PsiElementProcessor<ErlangNamedElement>() {
        @Override
        public boolean execute(@NotNull ErlangNamedElement component) {
          if (name.equals(component.getName())) {
            result.add(component);
          }
          return true;
        }
      });
    }
    return new ArrayList<ErlangNamedElement>(result);
  }

  private static class MyDataIndexer implements DataIndexer<String, Void, FileContent> {
    @Override
    @NotNull
    public Map<String, Void> map(final FileContent inputData) {
      final PsiFile psiFile = inputData.getPsiFile();
      final Map<String, Void> result = new THashMap<String, Void>();
      processComponents(psiFile, new PsiElementProcessor<ErlangNamedElement>() {
        @Override
        public boolean execute(@NotNull ErlangNamedElement component) {
          if (component.getName() != null) {
            result.put(component.getName(), null);
          }
          return true;
        }
      });
      return result;
    }
  }

  private static void processComponents(PsiFile psiFile, PsiElementProcessor<ErlangNamedElement> processor) {
    final ErlangNamedElement[] components = PsiTreeUtil.getChildrenOfType(psiFile, ErlangNamedElement.class);
    if (components == null) {
      return;
    }
    for (ErlangNamedElement component : components) {
      final String componentName = component.getName();
      if (componentName != null) {
        if (!processor.execute(component)) {
          return;
        }
      }
    }
  }
}