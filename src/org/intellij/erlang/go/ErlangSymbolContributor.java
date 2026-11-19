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

import com.intellij.navigation.GotoClassContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.ErlangStructureViewFactory;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.stubs.index.ErlangAllNameIndex;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ErlangSymbolContributor implements GotoClassContributor {
  private static final String SEPARATOR = ":";

  @NotNull
  @Override
  public String @NotNull [] getNames(Project project, boolean includeNonProjectItems) {
    return ArrayUtil.toStringArray(StubIndex.getInstance().getAllKeys(ErlangAllNameIndex.KEY, project));
  }

  @NotNull
  @Override
  public NavigationItem @NotNull [] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
    GlobalSearchScope scope = includeNonProjectItems ? GlobalSearchScope.allScope(project) : GlobalSearchScope.projectScope(project);
    Collection<ErlangNamedElement> result = StubIndex.getElements(ErlangAllNameIndex.KEY, name, project, scope, ErlangNamedElement.class);
    List<NavigationItem> items = new ArrayList<>(result.size());
    for (ErlangNamedElement element : result) {
      items.add(new ErlangSymbolNavigationItem(element));
    }
    return items.toArray(new NavigationItem[0]);
  }

  @Nullable
  @Override
  public String getQualifiedName(@NotNull NavigationItem item) {
    if (!(item instanceof ErlangSymbolNavigationItem symbol)) return null;
    String name = symbol.getName();
    PsiFile file = symbol.myElement.getContainingFile();
    if (name == null || file == null) return null;
    return FileUtilRt.getNameWithoutExtension(file.getName()) + SEPARATOR + name;
  }

  @NotNull
  @Override
  public String getQualifiedNameSeparator() {
    return SEPARATOR;
  }

  private static class ErlangSymbolNavigationItem extends ErlangStructureViewFactory.Element {
    private final ErlangNamedElement myElement;

    ErlangSymbolNavigationItem(@NotNull ErlangNamedElement element) {
      super(element);
      myElement = element;
    }

    @Override
    public String getLocationString() {
      return "(in " + myElement.getContainingFile().getName() + ")";
    }
  }
}
