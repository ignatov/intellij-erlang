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

import com.intellij.util.PathUtilRt;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;

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

  private void doSingleFileTest(String relativePath, String text, String expectedOutputFileName) {
    String depFile = createFile(relativePath, text);
    String moduleName = "m";
    addModule(moduleName, PathUtilRt.getParentPath(depFile));
    rebuildAll();
    assertCompiled(moduleName, expectedOutputFileName);
  }
}
