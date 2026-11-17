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

package org.intellij.erlang.jps;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.PathUtilRt;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsModule;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class ErlangBuilderTest extends ErlangJpsBuildTestCase {
  public void testSimple() {
    doSingleFileTest("src/simple.erl", "-module(simple). foo() -> ok.", "simple.beam");
  }

  public void testAppFilesAreCopiedToOutputDirectory() {
    doSingleFileTest("src/simple.app", "", "simple.app");
  }

  public void testAppSrcFilesAreCopiedToOutputDirectory() {
    doSingleFileTest("src/simple.app.src", "", "simple.app");
  }

  public void testAppSrcModulesAreFilledIn() throws IOException {
    createFile("src/zeta.erl", "-module(zeta).");
    createFile("src/alpha.erl", "-module(alpha).");
    String appSrc = createFile("src/simple.app.src", "%% comment\n{application, simple, [{vsn, \"1\"}, {modules, [old]}, {registered, []}]}.\n");
    addModule("m", PathUtilRt.getParentPath(appSrc));
    rebuildAll();
    assertEquals("{application,simple,[{vsn,\"1\"},{modules,[alpha,zeta]},{registered,[]}]}.\n", loadAppFile("m", "simple.app"));
  }

  public void testAppSrcModulesAreAddedWhenMissing() throws IOException {
    createFile("src/simple.erl", "-module(simple).");
    String appSrc = createFile("src/simple.app.src", "{application, simple, [{vsn, \"1\"}]}.");
    addModule("m", PathUtilRt.getParentPath(appSrc));
    rebuildAll();
    assertEquals("{application,simple,[{vsn,\"1\"},{modules,[simple]}]}.\n", loadAppFile("m", "simple.app"));
  }

  public void testAppSrcModulesFollowAddedAndRemovedModules() throws IOException {
    String simple = createFile("src/simple.erl", "-module(simple).");
    String appSrc = createFile("src/simple.app.src", "{application, simple, []}.");
    addModule("m", PathUtilRt.getParentPath(appSrc));
    rebuildAll();
    assertEquals("{application,simple,[{modules,[simple]}]}.\n", loadAppFile("m", "simple.app"));

    createFile("src/added.erl", "-module(added).");
    makeAll();
    assertEquals("{application,simple,[{modules,[added,simple]}]}.\n", loadAppFile("m", "simple.app"));

    FileUtil.delete(new File(simple));
    makeAll();
    assertEquals("{application,simple,[{modules,[added]}]}.\n", loadAppFile("m", "simple.app"));
  }

  public void testAppFilesAreCopiedUnchanged() throws IOException {
    String text = "{application, simple, [{modules, [old]}]}.";
    String app = createFile("src/simple.app", text);
    createFile("src/simple.erl", "-module(simple).");
    addModule("m", PathUtilRt.getParentPath(app));
    rebuildAll();
    assertEquals(text, loadAppFile("m", "simple.app"));
  }

  public void testAppSrcIsCopiedWithoutSdk() throws IOException {
    String text = "{application, simple, []}.";
    String appSrc = createFile("src/simple.app.src", text);
    addModule("m", PathUtilRt.getParentPath(appSrc));
    JpsModule module = myModel.getProject().getModules().get(0);
    module.getSdkReferencesTable().setSdkReference(JpsErlangSdkType.INSTANCE, null);
    BuildResult result = rebuildAllWithoutAssertions();
    result.assertSuccessful();
    assertTrue(ContainerUtil.exists(result.getWarnMessages(), m -> m.getMessageText().contains("No Erlang SDK for module m")));
    assertEquals(text, loadAppFile("m", "simple.app"));
  }

  public void testAppSrcIsCopiedWhenErlCannotBeStarted() throws IOException {
    String text = "{application, simple, []}.";
    String appSrc = createFile("src/simple.app.src", text);
    addModule("m", PathUtilRt.getParentPath(appSrc));
    String brokenHome = getAbsolutePath("no-erlang");
    JpsSdk<JpsDummyElement> brokenSdk = myModel.getGlobal().addSdk("broken", brokenHome, "R16B", JpsErlangSdkType.INSTANCE).getProperties();
    JpsModule module = myModel.getProject().getModules().get(0);
    module.getSdkReferencesTable().setSdkReference(JpsErlangSdkType.INSTANCE, brokenSdk.createReference());
    BuildResult result = rebuildAllWithoutAssertions();
    result.assertSuccessful();
    assertTrue(ContainerUtil.exists(result.getWarnMessages(), m -> m.getMessageText().contains("Failed to start erl")));
    assertEquals(text, loadAppFile("m", "simple.app"));
  }

  public void testWarningsDoNotFailBuild() {
    String file = createFile("src/simple.erl", "-module(simple). foo() -> ok.");
    addModule("m", PathUtilRt.getParentPath(file));
    BuildResult result = rebuildAllWithoutAssertions();
    result.assertSuccessful();
    assertTrue(ContainerUtil.exists(result.getWarnMessages(), m -> m.getMessageText().contains("function foo/0 is unused")));
    assertCompiled("m", "simple.beam");
  }

  public void testErlcFailureWithoutRecognizedErrorFailsBuild() {
    ErlangCompilerOptions options = setCompilerOptions();
    options.myAdditionalErlcArguments = List.of("+{bad");
    String file = createFile("src/simple.erl", "-module(simple).");
    addModule("m", PathUtilRt.getParentPath(file));
    BuildResult result = rebuildAllWithoutAssertions();
    assertTrue(ContainerUtil.exists(result.getErrorMessages(), m -> m.getMessageText().contains("erlc finished with exit code")));
  }

  public void testModuleWithoutRebarConfigIsCompiledWithErlcWhenRebarIsEnabled() {
    setCompilerOptions().myUseRebarCompiler = true;
    String file = createFile("src/simple.erl", "-module(simple).");
    addModule("m", PathUtilRt.getParentPath(file));
    BuildResult result = rebuildAllWithoutAssertions();
    result.assertSuccessful();
    assertTrue(ContainerUtil.exists(result.getWarnMessages(), m -> m.getMessageText().contains("rebar.config is not found in module 'm'")));
    assertCompiled("m", "simple.beam");
  }

  private String loadAppFile(String moduleName, String fileName) throws IOException {
    return FileUtil.loadFile(new File(getAbsolutePath("out/production/" + moduleName), fileName), StandardCharsets.UTF_8);
  }

  private void doSingleFileTest(String relativePath, String text, String expectedOutputFileName) {
    String depFile = createFile(relativePath, text);
    String moduleName = "m";
    addModule(moduleName, PathUtilRt.getParentPath(depFile));
    rebuildAll();
    assertCompiled(moduleName, expectedOutputFileName);
  }
}
