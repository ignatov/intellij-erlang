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

package org.intellij.erlang.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.roots.libraries.LibraryTablesRegistrar;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.util.ArrayUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangSdkForSmallIdes {
  private static final String LIBRARY_NAME = "Erlang SDK";

  private ErlangSdkForSmallIdes() {
  }

  public static void setUpOrUpdateSdk(@NotNull final Project project, @NotNull final String path) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        LibraryTable table = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
        Library get = table.getLibraryByName(LIBRARY_NAME);
        Library lib = get != null ? get : table.createLibrary(LIBRARY_NAME);

        Library.ModifiableModel libraryModel = lib.getModifiableModel();
        String libUrl = ArrayUtil.getFirstElement(lib.getUrls(OrderRootType.CLASSES));
        if (libUrl != null) {
          libraryModel.removeRoot(libUrl, OrderRootType.CLASSES);
        }

        String url = VfsUtilCore.pathToUrl(path);
        libraryModel.addRoot(url, OrderRootType.CLASSES);
        libraryModel.commit();

        boolean remove = path.isEmpty();
        if (remove) {
          updateModules(project, lib, true);
          table.removeLibrary(lib);
        }

        table.getModifiableModel().commit();

        if (!remove) {
          updateModules(project, lib, false);
        }
      }
    });
  }

  @Nullable
  static String getSdkHome(@NotNull final Project project) {
    return ApplicationManager.getApplication().runReadAction(new Computable<String>() {
      @Nullable
      @Override
      public String compute() {
        LibraryTable table = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
        Library lib = table.getLibraryByName(LIBRARY_NAME);
        String[] urls = lib == null ? ArrayUtil.EMPTY_STRING_ARRAY : lib.getUrls(OrderRootType.CLASSES);
        return VfsUtilCore.urlToPath(ArrayUtil.getFirstElement(urls));
      }
    });
  }

  private static void updateModules(@NotNull Project project, @NotNull Library lib, boolean remove) {
    Module[] modules = ModuleManager.getInstance(project).getModules();
    for (Module module : modules) {
      ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
      if (!remove) {
        if (model.findLibraryOrderEntry(lib) == null) {
          LibraryOrderEntry entry = model.addLibraryEntry(lib);
          entry.setScope(DependencyScope.PROVIDED);
        }
      }
      else {
        LibraryOrderEntry entry = model.findLibraryOrderEntry(lib);
        if (entry != null) {
          model.removeOrderEntry(entry);
        }
      }
      model.commit();
    }
  }
}
