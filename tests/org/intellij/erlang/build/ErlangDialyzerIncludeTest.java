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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.InspectionsKt;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.dialyzer.DialyzerSettings;
import org.intellij.erlang.dialyzer.ErlangDialyzerExternalAnnotator;
import org.intellij.erlang.dialyzer.ErlangDialyzerInspection;
import org.intellij.erlang.jps.model.ErlangIncludeSourceRootType;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;

import java.io.File;

// Runs the dialyzer of the Erlang SDK given by erlang.sdk.path, the way the dialyzer-based inspection runs it.
public class ErlangDialyzerIncludeTest extends ErlangCompilationTestBase {
  private static final String SPEC_MISMATCH = "Invalid type specification for function %s:test3/0";

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    InspectionsKt.enableInspectionTool(myProject, new ErlangDialyzerInspection(), getTestRootDisposable());
    DialyzerSettings.getInstance(myProject).setCurrentPltPath(buildPlt());
  }

  public void testSpecMismatchIsReported() throws Exception {
    VirtualFile file = addSourceFile(myModule, "n406.erl", """
      -module(n406).
      -export([test3/0]).

      -spec test3() -> integer().
      test3() -> ok.
      """);
    assertProblemOnLine(file, 4, String.format(SPEC_MISMATCH, "n406"));
  }

  // #406: a header from an include directory of the module (a rebar dependency's include/, as the rebar import marks it)
  // is not found by dialyzer, and nothing is reported. dialyzer itself finds only ../include, ../inc and ../incl.
  public void testSpecMismatchIsReportedInModuleWithHeaderFromIncludeDirectory() throws Exception {
    VirtualFile includeDir = WriteAction.computeAndWait(() -> {
      VirtualFile contentRoot = ModuleRootManager.getInstance(myModule).getContentRoots()[0];
      return VfsUtil.createDirectoryIfMissing(contentRoot, "deps/dep/include");
    });
    PsiTestUtil.addSourceRoot(myModule, includeDir, ErlangIncludeSourceRootType.INSTANCE);
    assertSameElements(ErlangIncludeDirectoryUtil.getIncludeDirectories(myModule), includeDir);
    addFileToDirectory(includeDir, "dep.hrl", "-define(VALUE, ok).\n");
    VirtualFile file = addSourceFile(myModule, "m406.erl", """
      -module(m406).
      -include("dep.hrl").
      -export([test3/0]).

      -spec test3() -> integer().
      test3() -> ?VALUE.
      """);
    assertProblemOnLine(file, 5, String.format(SPEC_MISMATCH, "m406"));
  }

  private void assertProblemOnLine(@NotNull VirtualFile file, int line, @NotNull String message) {
    PsiFile psiFile = PsiManager.getInstance(myProject).findFile(file);
    assertNotNull(psiFile);
    ErlangDialyzerExternalAnnotator annotator = new ErlangDialyzerExternalAnnotator();
    ErlangDialyzerExternalAnnotator.State state = annotator.collectInformation(psiFile);
    assertNotNull(state);
    annotator.doAnnotate(state);
    String problems = state.problems.toString();
    assertTrue("No problem on line " + line + " with '" + message + "' in " + problems,
               ContainerUtil.exists(state.problems, p -> p.toString().contains("myLine=" + line + ",") &&
                                                         p.toString().contains(message)));
  }

  @NotNull
  private String buildPlt() throws Exception {
    File dir = FileUtil.createTempDirectory("dialyzer", "plt");
    FileUtil.writeToFile(new File(dir, "plt406.erl"), "-module(plt406).\n");
    run(dir, "erlc", "+debug_info", "plt406.erl");
    File plt = new File(dir, "plt406.plt");
    run(dir, "dialyzer", "--build_plt", "--output_plt", plt.getPath(), "plt406.beam");
    assertTrue(plt.isFile());
    return plt.getPath();
  }

  private static void run(@NotNull File workDir, @NotNull String command, @NotNull String... parameters) throws Exception {
    GeneralCommandLine commandLine = new GeneralCommandLine(new File(JpsErlangSdkType.getTestsSdkPath(), "bin/" + command).getPath())
      .withParameters(parameters)
      .withWorkDirectory(workDir);
    ProcessOutput output = new CapturingProcessHandler(commandLine).runProcess(60000);
    assertFalse(output.getStdout() + output.getStderr(), output.isTimeout());
  }
}
