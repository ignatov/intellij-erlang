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
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectAndLibrariesScope;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangQAtom;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangModuleReferenceImpl extends ErlangQAtomBasedReferenceImpl {
  public ErlangModuleReferenceImpl(@NotNull PsiElement owner, @NotNull ErlangQAtom qAtom) {
    super(owner, qAtom, ErlangPsiImplUtil.getTextRangeForReference(owner, qAtom), ErlangPsiImplUtil.getNameIdentifier(qAtom).getText());
  }

  @Override
  public PsiElement resolveInner() {
    // the index order, the one the function of a remote call resolves by, so that dup and f of dup:f() pick one copy
    List<ErlangModule> modules = ErlangModuleIndex.getModulesByName(myElement.getProject(), myReferenceName, getSearchScope(myElement));
    return ContainerUtil.getFirstItem(modules);
  }

  @NotNull
  @Override
  public Object @NotNull [] getVariants() {
    return ArrayUtil.EMPTY_OBJECT_ARRAY;
  }

  @NotNull
  static GlobalSearchScope getSearchScope(@NotNull PsiElement element) {
    Project project = element.getProject();
    Module module = ModuleUtilCore.findModuleForPsiElement(element);
    return module != null ? module.getModuleWithDependenciesAndLibrariesScope(true) :
           new ProjectAndLibrariesScope(project);
  }
}
