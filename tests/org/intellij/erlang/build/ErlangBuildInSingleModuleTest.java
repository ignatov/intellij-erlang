/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.compiler.CompilerMessage;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.java.JavaSourceRootType;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.Assert.assertNotEquals;

public class ErlangBuildInSingleModuleTest extends ErlangCompilationTestBase {

  public void testBuildSingleFile() throws Exception {
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    compileAndAssertOutput(false);
  }

  public void testBuildWithTestSource() throws Exception {
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    addTestFile(myModule, "test1.erl", ErlangModuleTextGenerator.module("test1").build());
    compileAndAssertOutput(true);
  }

  public void testBuildWithParseTransform() throws Exception {
    addSourceFile(myModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").pt("parse_transform1").build());
    compileAndAssertOutput(false);
  }

  // #1028: with "Compile project with rebar" on, a module without rebar.config used to be skipped silently.
  public void testModuleWithoutRebarConfigIsCompiledWithErlcWhenRebarIsEnabled() throws Exception {
    ErlangCompilerSettings.getInstance(myProject).setUseRebarCompilerEnabled(true);
    addSourceFile(myModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").pt("parse_transform1").build());
    List<CompilerMessage> messages = myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    assertTrue(ContainerUtil.exists(messages, m -> m.getCategory() == CompilerMessageCategory.WARNING &&
                                                   m.getMessage().contains("rebar.config is not found")));
  }

  public void testBuildWithBehaviour() throws Exception {
    ErlangModuleTextGenerator.BehaviourBuilder behaviour = ErlangModuleTextGenerator.behaviour("behaviour1").callback("foo", 0);
    addSourceFile(myModule, "behaviour1.erl", behaviour.build());
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").behaviour(behaviour).build());
    compileAndAssertOutput(false);
  }

  public void testBuildWithIncludes() throws Exception {
    addSourceFile(myModule, "header.hrl", "");
    addSourceFile(myModule, "module2.erl", ErlangModuleTextGenerator.module("module2").include("header.hrl").build());
    myCompilationRunner.compile();
    compileAndAssertOutput(false);
  }

  public void testBuildWithStandardLibraryInclude() throws Exception {
    addSourceFile(myModule, "module2.erl", ErlangModuleTextGenerator.module("module2").includeLib("eunit/include/eunit.hrl").build());
    myCompilationRunner.compile();
    compileAndAssertOutput(false);
  }

  public void testRebuildWithNewFile() throws Exception {
    VirtualFile sourceFile = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    compileAndAssertOutput(false);
    long modificationTime = lastOutputModificationTime(myModule, sourceFile);
    addSourceFile(myModule, "module2.erl", ErlangModuleTextGenerator.module("module2").build());
    compileAndAssertOutput(false);
    assertEquals(modificationTime, lastOutputModificationTime(myModule, sourceFile));
  }

  public void testRebuildWithoutChanges() throws Exception {
    VirtualFile sourceFile1 = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    compileAndAssertOutput(false);
    long lastModificationTime1 = lastOutputModificationTime(myModule, sourceFile1);
    compileAndAssertOutput(false);
    assertEquals(lastModificationTime1, lastOutputModificationTime(myModule, sourceFile1));
  }

  public void testRebuildSingleFile() throws Exception {
    VirtualFile sourceFile = addSourceFile(myModule, "module2.erl", ErlangModuleTextGenerator.module("module2").build());
    compileAndAssertOutput(false);
    long lastModificationTime2 = lastOutputModificationTime(myModule, sourceFile);
    myCompilationRunner.touch(sourceFile);
    compileAndAssertOutput(false);
    assertNotEquals(sourceFile.getCanonicalPath() + " wasn't rebuilt after changes in file.",
                    lastModificationTime2,
                    lastOutputModificationTime(myModule, sourceFile));
  }

  public void testRebuildWithModuleWithoutDependencies() throws Exception {
    VirtualFile sourceFile1 = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    VirtualFile sourceFile2 = addSourceFile(myModule, "module2.erl", ErlangModuleTextGenerator.module("module2").build());
    compileAndAssertOutput(false);
    long lastModificationTime1 = lastOutputModificationTime(myModule, sourceFile1);
    myCompilationRunner.touch(sourceFile2);
    compileAndAssertOutput(false);
    assertEquals(sourceFile1.getCanonicalPath() + " was rebuilt without changes. Some file in project was changed.",
                 lastModificationTime1,
                 lastOutputModificationTime(myModule, sourceFile1));
  }

  public void testRebuildWithParseTransform() throws Exception {
    VirtualFile parseTransformSourceFile = addSourceFile(myModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").pt("parse_transform1").build());
    doTestRebuildInSingleModule(parseTransformSourceFile, sourceFileWithDependency);
  }

  public void testRebuildWithBehaviour() throws Exception {
    ErlangModuleTextGenerator.BehaviourBuilder behaviour = ErlangModuleTextGenerator.behaviour("behaviour1").callback("foo", 0);
    VirtualFile behaviourSourceFile = addSourceFile(myModule, "behaviour1.erl", behaviour.build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").behaviour(behaviour).build());
    doTestRebuildInSingleModule(behaviourSourceFile, sourceFileWithDependency);
  }

  public void testRebuildWithInclude() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").include("header.hrl").build());
    doTestRebuildInSingleModule(headerFile, sourceFileWithDependency);
  }

  public void testRebuildWithIncludesDirectory() throws Exception {
    VirtualFile includeSourceRoot = addIncludeRoot(myModule, "include");
    VirtualFile headerFile = addFileToDirectory(includeSourceRoot, "header.hrl", "");
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").include("header.hrl").build());
    doTestRebuildInSingleModule(headerFile, sourceFileWithDependency);
  }

  // #1410: a file above the code assistance size limit gets no Erlang PSI, so its includes can't be read.
  public void testRebuildLargeModuleWithInclude() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    VirtualFile largeModule = addSourceFile(myModule, "big.erl", aboveSizeLimit(ErlangModuleTextGenerator.module("big").include("header.hrl").build()));
    doTestRebuildInSingleModule(headerFile, largeModule);
  }

  public void testRebuildWithLargeInclude() throws Exception {
    VirtualFile largeHeader = addSourceFile(myModule, "big.hrl", aboveSizeLimit(""));
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").include("big.hrl").build());
    doTestRebuildInSingleModule(largeHeader, sourceFileWithDependency);
  }

  public void testBuildWithLargeHeaderAndParseTransformInHeader() throws Exception {
    addSourceFile(myModule, "h.hrl", "-compile({parse_transform, pt}).\n");
    addSourceFile(myModule, "pt.erl", ErlangModuleTextGenerator.pt("pt").build());
    addSourceFile(myModule, "m.erl", ErlangModuleTextGenerator.module("m").include("h.hrl").build());
    addSourceFile(myModule, "big.hrl", aboveSizeLimit("-define(X, 1)."));
    compileAndAssertOutput(false);
  }

  @NotNull
  private static String aboveSizeLimit(@NotNull String text) {
    StringBuilder builder = new StringBuilder(text).append('\n');
    String commentLine = "%" + "x".repeat(99) + "\n";
    while (builder.length() < 3_000_000) builder.append(commentLine);
    return builder.toString();
  }

  public void testRebuildWithTransitiveDependencies() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    ErlangModuleTextGenerator.BehaviourBuilder behaviour = ErlangModuleTextGenerator.behaviour("behaviour1").callback("foo", 0);
    addSourceFile(myModule, "behaviour1.erl", behaviour.include("header.hrl").build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").behaviour(behaviour).build());
    doTestRebuildInSingleModule(headerFile, sourceFileWithDependency);
  }

  public void testBeamsForDeletedSourcesAreDeleted() throws Exception {
    final VirtualFile erl = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());

    compileAndAssertOutput(false);
    File beam = getOutputFile(myModule, erl, false);
    assertNotNull(beam);
    assertTrue(beam.exists());

    WriteCommandAction.runWriteCommandAction(null, () -> {
      try {
        erl.delete(null);
      }
      catch (IOException e) {
        throw new RuntimeException(e);
      }
    });
    compileAndAssertOutput(false);
    assertFalse(beam.exists());
  }

  public void testAppSrcModulesSkipExcludedDirectory() throws Exception {
    addSourceFile(myModule, "one.erl", ErlangModuleTextGenerator.module("one").build());
    addSourceFile(myModule, "a.app.src", "{application, a, []}.");
    VirtualFile excluded = createSourceSubdirectory("excl");
    addFileToDirectory(excluded, "excl_mod.erl", ErlangModuleTextGenerator.module("excl_mod").build());
    PsiTestUtil.addExcludedRoot(myModule, excluded);
    myCompilationRunner.compile();
    assertEquals("{application,a,[{modules,[one]}]}.\n", loadOutputFile("a.app"));
  }

  public void testAppSrcModulesSkipNestedTestRoot() throws Exception {
    addSourceFile(myModule, "one.erl", ErlangModuleTextGenerator.module("one").build());
    addSourceFile(myModule, "a.app.src", "{application, a, []}.");
    VirtualFile nested = createSourceSubdirectory("nested");
    addFileToDirectory(nested, "one_tests.erl", ErlangModuleTextGenerator.module("one_tests").build());
    PsiTestUtil.addSourceRoot(myModule, nested, true);
    myCompilationRunner.compile();
    assertEquals("{application,a,[{modules,[one]}]}.\n", loadOutputFile("a.app"));
  }

  public void testAppSrcInTestRootFollowsAddedModules() throws Exception {
    addSourceFile(myModule, "one.erl", ErlangModuleTextGenerator.module("one").build());
    addTestFile(myModule, "a.app.src", "{application, a, []}.");
    myCompilationRunner.compile();
    assertEquals("{application,a,[{modules,[one]}]}.\n", loadOutputFile("a.app"));

    addSourceFile(myModule, "two.erl", ErlangModuleTextGenerator.module("two").build());
    myCompilationRunner.compile();
    assertEquals("{application,a,[{modules,[one,two]}]}.\n", loadOutputFile("a.app"));
  }

  private VirtualFile createSourceSubdirectory(String name) throws IOException {
    VirtualFile sourceRoot = ContainerUtil.getFirstItem(ModuleRootManager.getInstance(myModule).getSourceRoots(JavaSourceRootType.SOURCE));
    assertNotNull(sourceRoot);
    return WriteAction.computeAndWait(() -> sourceRoot.createChildDirectory(this, name));
  }

  private String loadOutputFile(String name) throws IOException {
    return FileUtil.loadFile(new File(getOutputDirectory(myModule, false), name), StandardCharsets.UTF_8);
  }

  private void doTestRebuildInSingleModule(VirtualFile dependency,
                                           VirtualFile sourceFileWithDependency) throws Exception {
    compileAndAssertOutput(false);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(dependency);
    compileAndAssertOutput(false);
    assertNotEquals(sourceFileWithDependency.getCanonicalPath() + " wasn't rebuilt after modification of its dependencies.",
                    sourceModificationTime,
                    lastOutputModificationTime(myModule, sourceFileWithDependency));
  }
}
