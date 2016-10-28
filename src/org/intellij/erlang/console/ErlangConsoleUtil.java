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

package org.intellij.erlang.console;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;

public final class ErlangConsoleUtil {
  public static final String EUNIT_FAILURE_PATH = "\\[\\{file,\"" + FileReferenceFilter.PATH_MACROS + "\"\\},\\{line," + FileReferenceFilter.LINE_MACROS + "\\}\\]";
  public static final String EUNIT_ERROR_PATH = FileReferenceFilter.PATH_MACROS + ", line " + FileReferenceFilter.LINE_MACROS;
  public static final String COMPILATION_ERROR_PATH = FileReferenceFilter.PATH_MACROS + ":" + FileReferenceFilter.LINE_MACROS;

  private ErlangConsoleUtil() {
  }

  public static void attachFilters(@NotNull Project project, @NotNull ConsoleView consoleView) {
    consoleView.addMessageFilter(new FileReferenceFilter(project, COMPILATION_ERROR_PATH));
    consoleView.addMessageFilter(new FileReferenceFilter(project, EUNIT_ERROR_PATH));
    consoleView.addMessageFilter(new FileReferenceFilter(project, EUNIT_FAILURE_PATH));
  }

  @NotNull
  public static List<String> getCodePath(@NotNull Module module, boolean forTests, boolean useRebarPaths) {
    return getCodePath(module.getProject(), module, forTests, useRebarPaths);
  }

  @NotNull
  public static List<String> getCodePath(@NotNull Project project, @Nullable Module module, boolean useTestOutputPath,
                                         boolean useRebarPaths) {
    final Set<Module> codePathModules = new HashSet<>();
    if (module != null) {
      ModuleRootManager moduleRootMgr = ModuleRootManager.getInstance(module);
      moduleRootMgr.orderEntries().recursively().forEachModule(dependencyModule -> {
        codePathModules.add(dependencyModule);
        return true;
      });
    }
    else {
      codePathModules.addAll(Arrays.asList(ModuleManager.getInstance(project).getModules()));
    }

    List<String> codePath = new ArrayList<>(codePathModules.size() * 2);

    if (module != null) {
      for (VirtualFile contentRoot : ModuleRootManager.getInstance(module).getContentRoots()) {
        codePath.add("-pa");
        codePath.add(new File(contentRoot.getPath(), ".eunit").toString());
      }
    }

    for (Module codePathModule : codePathModules) {
      if (useRebarPaths) {
        for (VirtualFile contentRoot : ModuleRootManager.getInstance(codePathModule).getContentRoots()) {
          codePath.add("-pa");
          codePath.add(new File(contentRoot.getPath(), "ebin").toString());
        }
      } else {
        ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(codePathModule);
        CompilerModuleExtension compilerModuleExt =
          moduleRootManager.getModuleExtension(CompilerModuleExtension.class);
        VirtualFile buildOutput = useTestOutputPath && codePathModule == module ?
          getCompilerOutputPathForTests(compilerModuleExt) :
          compilerModuleExt.getCompilerOutputPath();
        if (buildOutput != null) {
          codePath.add("-pa");
          codePath.add(buildOutput.getCanonicalPath());
        }
        for (VirtualFile contentRoot : ModuleRootManager.getInstance(codePathModule).getContentRoots()) {
          codePath.add("-pa");
          codePath.add(contentRoot.getPath());
        }
      }
    }

    return codePath;
  }

  @Nullable
  private static VirtualFile getCompilerOutputPathForTests(CompilerModuleExtension module) {
    VirtualFile testPath = module.getCompilerOutputPathForTests();
    return testPath == null || !testPath.exists() ? module.getCompilerOutputPath() : testPath;
  }

  @NotNull
  static String getWorkingDirPath(@NotNull Project project, @NotNull String workingDirPath) {
    if (workingDirPath.isEmpty()) {
      return project.getBasePath();
    }
    return workingDirPath;
  }

  @NotNull
  static String getErlPath(@NotNull Project project, @Nullable Module module) throws ExecutionException {
    Sdk sdk;
    if (module != null) {
      sdk = ModuleRootManager.getInstance(module).getSdk();
    }
    else {
      sdk = ProjectRootManager.getInstance(project).getProjectSdk();
    }
    if (sdk == null) {
      throw new ExecutionException("Erlang SDK is not configured");
    }
    return sdk.getHomePath() + File.separator + "bin" + File.separator + "erl";
  }
}

