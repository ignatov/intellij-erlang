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


package org.intellij.erlang.build;

import com.intellij.openapi.compiler.CompilerMessage;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.jps.rebar.RebarBuilder;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Builds a rebar3 application through the external build process with "Compile project with rebar" turned on.
 * The tests need the rebar3 escript in the rebar.path system property and are skipped without it.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ErlangRebarBuildTest extends ErlangCompilationTestBase {
  private static final String APP_SRC = "{application, app1, [{description, \"app1\"}, {vsn, \"1.0.0\"}, {registered, []}, " +
                                        "{applications, [kernel, stdlib]}, {env, []}]}.";

  public void testRebar3BuildsApplicationIntoBuildDirectory() throws Exception {
    VirtualFile contentRoot = setUpRebarApp();
    addSourceFile(myModule, "app1_mod.erl", ErlangModuleTextGenerator.module("app1_mod").build());
    myCompilationRunner.compile();

    File ebin = new File(contentRoot.getPath(), "_build/default/lib/app1/ebin");
    assertSameElements(listFiles(ebin), "app1.app", "app1_mod.beam");
    // The module output stays empty: rebar3 output isn't copied there, so run configurations that put the module
    // output on the code path don't find these modules (#1016).
    assertEmpty(listFiles(getOutputDirectory(myModule, false)));
  }

  public void testRebar3CompileErrorFailsBuild() throws Exception {
    VirtualFile contentRoot = setUpRebarApp();
    addSourceFile(myModule, "app1_mod.erl", ErlangModuleTextGenerator.module("app1_mod").build());
    addSourceFile(myModule, "bad.erl", "-module(bad).\nfoo( -> ok.\n");
    List<CompilerMessage> messages = myCompilationRunner.compileWithoutAssertions();

    assertTrue(messages.toString(), ContainerUtil.exists(messages, m -> m.getCategory() == CompilerMessageCategory.ERROR &&
                                                                        m.getMessage().contains("Rebar process finished with exit code 1")));
    assertTrue(messages.toString(), ContainerUtil.exists(messages, m -> m.getMessage().contains("syntax error before: '->'")));
    assertFalse(new File(contentRoot.getPath(), "_build/default/lib/app1/ebin/bad.beam").exists());
  }

  @NotNull
  private VirtualFile setUpRebarApp() throws IOException {
    String rebarPath = RebarBuilder.getTestsRebarPath();
    Assume.assumeTrue("Set the rebar.path system property to the rebar3 escript to run this test", rebarPath != null);
    assertTrue("rebar.path is set to " + rebarPath + ", which is not a file", new File(rebarPath).isFile());

    ErlangCompilerSettings.getInstance(myProject).setUseRebarCompilerEnabled(true);
    RebarSettings.getInstance(myProject).setRebarPath(rebarPath);
    VirtualFile[] contentRoots = ModuleRootManager.getInstance(myModule).getContentRoots();
    assertSize(1, contentRoots);
    addFileToDirectory(contentRoots[0], "rebar.config", "{erl_opts, [debug_info]}.\n");
    addSourceFile(myModule, "app1.app.src", APP_SRC);
    return contentRoots[0];
  }

  @NotNull
  private static List<String> listFiles(@NotNull File directory) {
    String[] names = directory.list();
    return names != null ? Arrays.asList(names) : List.of();
  }
}
