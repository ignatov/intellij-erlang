/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.compilation;

import com.intellij.compiler.server.CompileServerPlugin;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.extensions.PluginDescriptor;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.jps.rebar.RebarBuilder;
import org.intellij.erlang.module.ErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

final class ErlangCompilationUtil {
  static final PluginId ERLANG_PLUGIN_ID = PluginId.getId("org.jetbrains.erlang");

  private ErlangCompilationUtil() {
  }

  static boolean hasErlangModules(@NotNull CompileScope scope) {
    for (Module module : scope.getAffectedModules()) {
      if (isErlangModule(module)) return true;
    }
    return false;
  }

  /**
   * Returns true if the scope contains Erlang modules that the erlc builder compiles. With "Compile project with rebar"
   * turned on, rebar compiles only the modules that have rebar.config, and erlc compiles the others.
   */
  static boolean hasModulesCompiledWithErlc(@NotNull Project project, @NotNull CompileScope scope) {
    boolean useRebar = ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled();
    return ApplicationManager.getApplication().runReadAction((Computable<Boolean>) () -> {
      for (Module module : scope.getAffectedModules()) {
        if (isErlangModule(module) && (!useRebar || !hasRebarConfig(module))) return true;
      }
      return false;
    });
  }

  private static boolean isErlangModule(@NotNull Module module) {
    return ModuleType.get(module) instanceof ErlangModuleType;
  }

  private static boolean hasRebarConfig(@NotNull Module module) {
    for (VirtualFile contentRoot : ModuleRootManager.getInstance(module).getContentRoots()) {
      VirtualFile rebarConfig = contentRoot.findChild(RebarBuilder.REBAR_CONFIG_FILE_NAME);
      if (rebarConfig != null && !rebarConfig.isDirectory()) return true;
    }
    return false;
  }

  /**
   * Checks that the external build process can load the Erlang JPS plugin. If it can't, the build process neither knows
   * Erlang modules nor runs erlc and the build "succeeds" after copying .erl files to the output directory (#1077).
   *
   * @return a description of the problem, or null if the plugin is registered and its jars exist
   */
  @Nullable
  static String findCompileServerPluginProblem() {
    for (CompileServerPlugin serverPlugin : CompileServerPlugin.EP_NAME.getExtensionList()) {
      PluginDescriptor descriptor = serverPlugin.getPluginDescriptor();
      if (descriptor == null || !ERLANG_PLUGIN_ID.equals(descriptor.getPluginId())) continue;
      List<Path> missing = findMissingCompileServerJars(descriptor.getPluginPath(), serverPlugin.getClasspath());
      return missing.isEmpty() ? null : "The Erlang build process plugin is not found at " + StringUtil.join(missing, Path::toString, ", ");
    }
    return "The Erlang plugin does not register its build process plugin";
  }

  /**
   * Resolves the {@code compileServer.plugin} classpath the way the IDE does it for an installed plugin: the entries are
   * relative to the {@code lib} directory of the plugin, and a plugin packaged as a single jar contributes that jar.
   */
  @NotNull
  static List<Path> findMissingCompileServerJars(@Nullable Path pluginPath, @Nullable String classpath) {
    List<Path> missing = new ArrayList<>();
    if (pluginPath == null || Files.isRegularFile(pluginPath) || Files.isDirectory(pluginPath.resolve("classes"))) {
      return missing;
    }
    List<String> entries = StringUtil.split(StringUtil.notNullize(classpath), ";");
    if (entries.isEmpty()) {
      missing.add(pluginPath.resolve("lib"));
    }
    for (String entry : entries) {
      Path jar = pluginPath.resolve("lib").resolve(entry);
      if (!Files.exists(jar)) {
        missing.add(jar);
      }
    }
    return missing;
  }
}
