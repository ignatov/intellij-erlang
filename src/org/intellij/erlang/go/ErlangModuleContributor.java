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

package org.intellij.erlang.go;

import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.ErlangStructureViewFactory;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class ErlangModuleContributor implements ChooseByNameContributor {
  @NotNull
  @Override
  public NavigationItem[] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
    GlobalSearchScope scope = includeNonProjectItems ? GlobalSearchScope.allScope(project) : GlobalSearchScope.projectScope(project);
    List<ErlangModule> result = ErlangModuleIndex.getModulesByName(project, name, scope);
    ArrayList<NavigationItem> items = new ArrayList<>(result.size());
    for (ErlangNamedElement element : result) {
      PsiFile containingFile = element.getContainingFile();
      VirtualFile virtualFile = containingFile != null ? containingFile.getVirtualFile() : null;
      String moduleName = virtualFile != null ? virtualFile.getNameWithoutExtension() : null;
      items.add(new ErlangModuleNavigationItem(element, moduleName == null ? element.getName() : moduleName));
    }
    return items.toArray(new NavigationItem[items.size()]);
  }

  @NotNull
  @Override
  public String[] getNames(Project project, boolean includeNonProjectItems) {
    return ArrayUtil.toStringArray(ErlangModuleIndex.getNames(project));
  }

  private static class ErlangModuleNavigationItem extends ErlangStructureViewFactory.Element {
    private final String myPresentableName;

    public ErlangModuleNavigationItem(@NotNull PsiElement element, @Nullable String presentableName) {
      super(element);
      myPresentableName = presentableName;
    }

    @Nullable
    @Override
    public String getName() {
      return myPresentableName;
    }

    @Override
    public String getPresentableText() {
      return myPresentableName;
    }
  }
}
