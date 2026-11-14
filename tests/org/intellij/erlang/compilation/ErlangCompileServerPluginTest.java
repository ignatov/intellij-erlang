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
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.util.containers.ContainerUtil;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * The build process loads the Erlang JPS plugin from the jars named in {@code compileServer.plugin}. If a name doesn't
 * match the packaged jar, builds copy .erl files to the output directory and report success (#1077).
 */
public class ErlangCompileServerPluginTest extends BasePlatformTestCase {
  public void testRegisteredJpsPluginJarsExistInInstalledPlugin() {
    CompileServerPlugin plugin = ContainerUtil.find(CompileServerPlugin.EP_NAME.getExtensionList(),
                                                    p -> ErlangCompilationUtil.ERLANG_PLUGIN_ID.equals(p.getPluginDescriptor().getPluginId()));
    assertNotNull("compileServer.plugin is not registered", plugin);
    Path pluginPath = plugin.getPluginDescriptor().getPluginPath();
    assertTrue("Expected the plugin to be installed as a directory: " + pluginPath, Files.isDirectory(pluginPath.resolve("lib")));
    assertEmpty(ErlangCompilationUtil.findMissingCompileServerJars(pluginPath, plugin.getClasspath()));
    assertNull(ErlangCompilationUtil.findCompileServerPluginProblem());
  }

  public void testMissingJarIsReported() throws IOException {
    Path pluginPath = Files.createDirectories(Files.createTempDirectory("erlang-plugin").resolve("intellij-erlang"));
    Files.createDirectories(pluginPath.resolve("lib"));
    Files.createFile(pluginPath.resolve("lib/intellij-erlang.jps-plugin.jar"));

    assertEmpty(ErlangCompilationUtil.findMissingCompileServerJars(pluginPath, "intellij-erlang.jps-plugin.jar"));
    // 0.11.1266 and 0.11.1267 registered jps-plugin.jar but shipped lib/intellij-erlang.jps-plugin.jar.
    List<Path> missing = ErlangCompilationUtil.findMissingCompileServerJars(pluginPath, "jps-plugin.jar");
    assertEquals(List.of(pluginPath.resolve("lib/jps-plugin.jar")), missing);
    assertEquals(List.of(pluginPath.resolve("lib")), ErlangCompilationUtil.findMissingCompileServerJars(pluginPath, ""));
  }
}
