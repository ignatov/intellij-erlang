/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getModule;

/**
 * @author ignatov
 */
public class ErlangModuleReferenceImpl<T extends ErlangQAtom> extends ErlangAtomBasedReferenceImpl<T> {
  public ErlangModuleReferenceImpl(@NotNull T element, TextRange range, String name) {
    super(element, range, name);
  }

  @Override
  public PsiElement resolve() {
    PsiFile[] files = FilenameIndex.getFilesByName(myElement.getProject(), myReferenceName, GlobalSearchScope.allScope(myElement.getProject()));
    for (PsiFile file : files) {
      ErlangModule module = getModule(file);
      if (module != null) return module;
    }
    return null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(myElement.getContainingFile()));
  }
}
