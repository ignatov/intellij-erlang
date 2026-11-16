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
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.ErlangStructureViewFactory;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.stubs.index.ErlangAllNameIndex;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Go to Symbol from the plugin's stubs. In a project whose roots ELP serves all, ELP's {@code workspace/symbol} answers through the
 * platform's LSP client and this contributor is silent; otherwise it leaves out the symbols of files under a root that ELP serves.
 */
public class ErlangSymbolContributor implements ChooseByNameContributor {
  @NotNull
  @Override
  public String @NotNull [] getNames(Project project, boolean includeNonProjectItems) {
    if (ElpEngine.servesWholeProject(project)) return ArrayUtil.EMPTY_STRING_ARRAY;
    return ArrayUtil.toStringArray(StubIndex.getInstance().getAllKeys(ErlangAllNameIndex.KEY, project));
  }

  @NotNull
  @Override
  public NavigationItem @NotNull [] getItemsByName(String name, String pattern, Project project, boolean includeNonProjectItems) {
    if (ElpEngine.servesWholeProject(project)) return NavigationItem.EMPTY_NAVIGATION_ITEM_ARRAY;
    GlobalSearchScope scope = includeNonProjectItems ? GlobalSearchScope.allScope(project) : GlobalSearchScope.projectScope(project);
    Collection<ErlangNamedElement> result = StubIndex.getElements(ErlangAllNameIndex.KEY, name, project, scope, ErlangNamedElement.class);
    List<NavigationItem> items = new ArrayList<>(result.size());
    for (ErlangNamedElement element : result) {
      if (ElpEngine.ownsFeature(element, ElpEngine.Feature.WORKSPACE_SYMBOLS)) continue;
      items.add(new ErlangStructureViewFactory.Element(element) {
        @Override
        public String getLocationString() {
          return "(in " + element.getContainingFile().getName() + ")";
        }
      });
    }
    return items.toArray(new NavigationItem[0]);
  }
}
