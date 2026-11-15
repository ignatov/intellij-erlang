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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.jps.rebar.RebarBuilder;
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
  public static List<String> getCodePath(@NotNull Module module, boolean forTests) {
    return getCodePath(module.getProject(), module, forTests);
  }

  @NotNull
  public static List<String> getCodePath(@NotNull Project project, @Nullable Module module, boolean useTestOutputPath) {
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

    boolean useRebarBuild = ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled();
    List<String> codePath = new ArrayList<>(codePathModules.size() * 2);
    for (Module codePathModule : codePathModules) {
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
      String rebar3Output = useRebarBuild ? getRebar3OutputPath(codePathModule) : null;
      if (rebar3Output != null && (buildOutput == null || !FileUtil.pathsEqual(rebar3Output, buildOutput.getPath()))) {
        codePath.add("-pa");
        codePath.add(rebar3Output);
      }
      for (VirtualFile contentRoot : ModuleRootManager.getInstance(codePathModule).getContentRoots()) {
        codePath.add("-pa");
        codePath.add(contentRoot.getPath());
      }
    }

    return codePath;
  }

  /**
   * The directory where the Rebar build writes the module's beams. It runs `rebar3 compile` in each content root that
   * has rebar.config, and rebar3 writes into _build/default/lib/&lt;app&gt;/ebin there, where &lt;app&gt; is the name
   * of the application resource file, which may differ from the module name. The directory is checked on disk,
   * because the IDE may not know about _build yet when it was created by the build that ran just before the launch.
   */
  @Nullable
  private static String getRebar3OutputPath(@NotNull Module module) {
    for (VirtualFile contentRoot : ModuleRootManager.getInstance(module).getContentRoots()) {
      File root = VfsUtilCore.virtualToIoFile(contentRoot);
      if (!new File(root, RebarBuilder.REBAR_CONFIG_FILE_NAME).isFile()) continue;
      for (String appName : getApplicationNames(root, module.getName())) {
        File ebin = new File(root, "_build/default/lib/" + appName + "/ebin");
        if (ebin.isDirectory()) return FileUtil.toSystemIndependentName(ebin.getPath());
      }
    }
    return null;
  }

  @NotNull
  private static Set<String> getApplicationNames(@NotNull File root, @NotNull String moduleName) {
    Set<String> appNames = new LinkedHashSet<>();
    addApplicationNames(appNames, new File(root, "src"), ".app.src");
    addApplicationNames(appNames, new File(root, "ebin"), ".app");
    appNames.add(moduleName);
    return appNames;
  }

  private static void addApplicationNames(@NotNull Set<String> appNames, @NotNull File dir, @NotNull String extension) {
    String[] fileNames = dir.list();
    if (fileNames == null) return;
    Arrays.sort(fileNames);
    for (String fileName : fileNames) {
      if (fileName.endsWith(extension) && fileName.length() > extension.length()) {
        appNames.add(fileName.substring(0, fileName.length() - extension.length()));
      }
    }
  }

  @Nullable
  private static VirtualFile getCompilerOutputPathForTests(CompilerModuleExtension module) {
    VirtualFile testPath = module.getCompilerOutputPathForTests();
    return testPath == null || !testPath.exists() ? module.getCompilerOutputPath() : testPath;
  }

  @NotNull
  static String getWorkingDirPath(@NotNull Project project, @NotNull String workingDirPath) {
    if (workingDirPath.isEmpty()) {
      return ObjectUtils.notNull(project.getBasePath(), "");
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

