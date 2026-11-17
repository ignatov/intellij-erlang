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

package org.intellij.erlang.hierarchy;

import com.intellij.ide.hierarchy.HierarchyNodeDescriptor;
import com.intellij.ide.hierarchy.HierarchyTreeStructure;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtilRt;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class ErlangCallerMethodsTreeStructure extends HierarchyTreeStructure {
  private final String myCurrentScopeType;

  public ErlangCallerMethodsTreeStructure(Project project, ErlangFunction function, String currentScopeType) {
    super(project, new ErlangFunctionNodeDescriptor(project, function));
    myCurrentScopeType = currentScopeType;
  }

  @NotNull
  @Override
  protected Object @NotNull [] buildChildren(@NotNull HierarchyNodeDescriptor descriptor) {
    ErlangFunction function = ObjectUtils.tryCast(descriptor.getPsiElement(), ErlangFunction.class);
    if (function == null) return ArrayUtilRt.EMPTY_OBJECT_ARRAY;
    SearchScope searchScope = getSearchScope(myCurrentScopeType, function);
    Set<ErlangFunction> result = new HashSet<>();
    ReferencesSearch.search(function, searchScope).forEach(
      (Consumer<? super PsiReference>) r -> {
        ProgressManager.checkCanceled();
        PsiElement element = r.getElement();
        ErlangFunction f = PsiTreeUtil.getParentOfType(element, ErlangFunction.class, false);
        ContainerUtil.addIfNotNull(result, f);
      }
    );
    return result.stream().map(f -> new ErlangFunctionNodeDescriptor(myProject, f)).toArray();
  }
}
