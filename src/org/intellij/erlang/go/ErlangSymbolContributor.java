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

import com.intellij.navigation.ChooseByNameContributorEx;
import com.intellij.navigation.GotoClassContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.Processor;
import com.intellij.util.indexing.FindSymbolParameters;
import com.intellij.util.indexing.IdFilter;
import org.intellij.erlang.ErlangStructureViewFactory;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.stubs.index.ErlangAllNameIndex;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangSymbolContributor implements GotoClassContributor, ChooseByNameContributorEx {
  private static final String SEPARATOR = ":";

  @Override
  public void processNames(@NotNull Processor<? super String> processor, @NotNull GlobalSearchScope scope, @Nullable IdFilter filter) {
    StubIndex.getInstance().processAllKeys(ErlangAllNameIndex.KEY, processor, scope, filter);
  }

  @Override
  public void processElementsWithName(@NotNull String name,
                                      @NotNull Processor<? super NavigationItem> processor,
                                      @NotNull FindSymbolParameters parameters) {
    GlobalSearchScope scope = parameters.getSearchScope();
    String module = getExactModuleQualifier(parameters, scope);
    StubIndex.getInstance().processElements(ErlangAllNameIndex.KEY, name, parameters.getProject(), scope, parameters.getIdFilter(),
                                            ErlangNamedElement.class, element -> {
        if (module != null && !isInModule(element, module)) return true;
        return processor.process(new ErlangSymbolNavigationItem(element));
      });
  }

  // "module:name", where module is the exact name of a module in the scope, lists only the symbols of that module
  // and of the headers with the same name, the files that getQualifiedName gives the qualifier "module".
  // Any other qualifier is left to the fuzzy matching of the qualified name.
  @Nullable
  private static String getExactModuleQualifier(@NotNull FindSymbolParameters parameters, @NotNull GlobalSearchScope scope) {
    String pattern = parameters.getCompletePattern().trim();
    int separator = pattern.indexOf(SEPARATOR);
    if (separator <= 0 || separator + 1 >= pattern.length()) return null;
    if (Character.isDigit(pattern.charAt(separator + 1))) return null; // "name:12" goes to a line
    String module = pattern.substring(0, separator);
    if (StringUtil.containsAnyChar(module, "* ")) return null;
    return ErlangModuleIndex.hasModule(module, scope) ? module : null;
  }

  private static boolean isInModule(@NotNull ErlangNamedElement element, @NotNull String module) {
    PsiFile file = element.getContainingFile();
    return file != null && module.equals(FileUtilRt.getNameWithoutExtension(file.getName()));
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
