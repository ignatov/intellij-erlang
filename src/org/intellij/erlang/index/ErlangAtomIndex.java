/*
 * Copyright 2012-2015 Sergey Ignatov
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
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.CommonProcessors;
import com.intellij.util.indexing.*;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.KeyDescriptor;
import gnu.trove.THashMap;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Map;

public class ErlangAtomIndex extends ScalarIndexExtension<String> {
  private static final ID<String, Void> ERLANG_ATOM_INDEX = ID.create("erlang.atom.index");
  private static final int INDEX_VERSION = 1;

  @NotNull
  @Override
  public ID<String, Void> getName() {
    return ERLANG_ATOM_INDEX;
  }

  @Override
  public int getVersion() {
    return INDEX_VERSION;
  }

  @NotNull
  @Override
  public DataIndexer<String, Void, FileContent> getIndexer() {
    return inputData -> {
      final Map<String, Void> result = new THashMap<String, Void>();
      PsiFile file = inputData.getPsiFile();
      if (file instanceof ErlangFile) {
        file.accept(new ErlangRecursiveVisitor() {
          @Override
          public void visitQAtom(@NotNull ErlangQAtom o) {
            if (ErlangPsiImplUtil.standaloneAtom(o)) result.put(o.getText(), null);
          }
        });
      }
      return result;
    };
  }

  @NotNull
  @Override
  public KeyDescriptor<String> getKeyDescriptor() {
    return new EnumeratorStringDescriptor();
  }

  @NotNull
  @Override
  public FileBasedIndex.InputFilter getInputFilter() {
    return ErlangIndexUtil.ERLANG_MODULE_FILTER;
  }

  @Override
  public boolean dependsOnFileContent() {
    return true;
  }

  @Override
  public boolean traceKeyHashToVirtualFileMapping() {
    return true;
  }

  @NotNull
  public static Collection<String> getNames(@NotNull Project project, @NotNull GlobalSearchScope searchScope) {
    CommonProcessors.CollectUniquesProcessor<String> processor = new CommonProcessors.CollectUniquesProcessor<String>();
    FileBasedIndex.getInstance().processAllKeys(ERLANG_ATOM_INDEX, processor, searchScope, IdFilter.getProjectIdFilter(project, false));
    return processor.getResults();
  }
}
