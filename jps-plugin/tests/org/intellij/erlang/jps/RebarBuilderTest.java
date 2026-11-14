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


package org.intellij.erlang.jps;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.jps.rebar.JpsRebarConfigurationExtension;
import org.intellij.erlang.jps.rebar.RebarBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.util.JpsPathUtil;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

/**
 * Builds a rebar3 application with "Compile project with rebar" turned on. The tests that run rebar need the rebar3
 * escript in the rebar.path system property and are skipped without it.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class RebarBuilderTest extends ErlangJpsBuildTestCase {
  private static final String APP_SRC = "{application, app1, [{description, \"app1\"}, {vsn, \"1.0.0\"}, {registered, []}, " +
                                        "{applications, [kernel, stdlib]}, {env, []}]}.";
  private static final String MODULE = "-module(app1_mod).\n-export([hello/0]).\nhello() -> ok.\n";

  public void testRebar3CompilesApplicationIntoBuildDirectory() {
    useRebar(requireRebarPath());
    createRebarApp("{erl_opts, []}.");
    rebuildAll();

    // rebar3 writes the application to _build/default/lib/<app>/ebin.
    assertSameElements(listFiles(appPath("_build/default/lib/app1/ebin")), "app1.app", "app1_mod.beam");
    // erlc doesn't compile the module and nothing is copied to the module output directory, so the IDE run
    // configurations, which put the module output on the code path, don't see these beams (#1016).
    assertEmpty(listFiles(getAbsolutePath("out/production/m")));
  }

  public void testDebugInfoOptionAddsDebugInfoToRebarBuild() throws IOException {
    String rebarPath = requireRebarPath();
    createRebarApp("{erl_opts, [no_debug_info]}.");
    String beam = appPath("_build/default/lib/app1/ebin/app1_mod.beam");

    useRebar(rebarPath).myAddDebugInfoEnabled = false;
    rebuildAll();
    assertFalse("erl_opts has no_debug_info", hasAbstractCode(beam));

    FileUtil.delete(new File(appPath("_build")));
    useRebar(rebarPath).myAddDebugInfoEnabled = true;
    rebuildAll();
    assertTrue("'Add debug info' should override no_debug_info in erl_opts", hasAbstractCode(beam));
  }

  public void testRebar3CompileErrorFailsBuild() {
    useRebar(requireRebarPath());
    createRebarApp("{erl_opts, []}.");
    createFile("app1/src/bad.erl", "-module(bad).\nfoo( -> ok.\n");
    BuildResult result = rebuildAllWithoutAssertions();

    assertTrue(ContainerUtil.exists(result.getErrorMessages(), m -> m.getMessageText().contains("Rebar process finished with exit code 1")));
    // rebar3 prints compiler errors in its multi-line "rich" format, which the builder reports as an info message.
    assertTrue(ContainerUtil.exists(result.getInfoMessages(), m -> m.getMessageText().contains("syntax error before: '->'")));
    assertFalse(new File(appPath("_build/default/lib/app1/ebin/bad.beam")).exists());
  }

  public void testRebarPathNotSetFailsBuild() {
    useRebar("");
    createRebarApp("{erl_opts, []}.");
    BuildResult result = rebuildAllWithoutAssertions();
    assertTrue(ContainerUtil.exists(result.getErrorMessages(), m -> m.getMessageText().equals("Rebar path is not set")));
    assertEmpty(listFiles(getAbsolutePath("out/production/m")));
  }

  @NotNull
  private static String requireRebarPath() {
    String rebarPath = RebarBuilder.getTestsRebarPath();
    Assume.assumeTrue("Set the rebar.path system property to the rebar3 escript to run this test", rebarPath != null);
    assertTrue("rebar.path is set to " + rebarPath + ", which is not a file", new File(rebarPath).isFile());
    return rebarPath;
  }

  @NotNull
  private ErlangCompilerOptions useRebar(@NotNull String rebarPath) {
    ErlangCompilerOptions options = setCompilerOptions();
    options.myUseRebarCompiler = true;
    JpsRebarConfigurationExtension.getOrCreateExtension(myModel.getProject()).getRebarSettingsState().myRebarPath = rebarPath;
    return options;
  }

  // An OTP application in app1/ with rebar.config, src/app1.app.src and src/app1_mod.erl. Module "m" has the content
  // root app1/ and the source root app1/src, as after the rebar import, and the output directory out/production/m.
  private void createRebarApp(@NotNull String rebarConfig) {
    String rebarConfigPath = createFile("app1/rebar.config", rebarConfig);
    String appDir = new File(rebarConfigPath).getParent();
    createFile("app1/src/app1.app.src", APP_SRC);
    createFile("app1/src/app1_mod.erl", MODULE);
    addModule("m", appDir + "/src");
    JpsModule module = myModel.getProject().findModuleByName("m");
    assertNotNull(module);
    module.getContentRootsList().removeUrl(JpsPathUtil.pathToUrl(appDir + "/src"));
    module.getContentRootsList().addUrl(JpsPathUtil.pathToUrl(appDir));
  }

  @NotNull
  private String appPath(@NotNull String relativePath) {
    return getAbsolutePath("app1/" + relativePath);
  }

  @NotNull
  private static List<String> listFiles(@NotNull String directory) {
    String[] names = new File(directory).list();
    return names != null ? Arrays.asList(names) : List.of();
  }

  private static boolean hasAbstractCode(@NotNull String beamPath) throws IOException {
    String erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(getErlangSdkPath()).getAbsolutePath();
    String eval = "[F] = init:get_plain_arguments(), {ok, {_, [{abstract_code, A}]}} = beam_lib:chunks(F, [abstract_code]), " +
                  "io:put_chars(atom_to_list(A =/= no_abstract_code)), halt().";
    Process process = new ProcessBuilder(erl, "-noshell", "-eval", eval, "-extra", beamPath).redirectErrorStream(true).start();
    try {
      String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
      assertEquals("erl output: " + output, 0, process.waitFor());
      assertTrue("erl output: " + output, output.equals("true") || output.equals("false"));
      return Boolean.parseBoolean(output);
    }
    catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new IOException(e);
    }
  }
}
