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

package org.intellij.erlang.compilation;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleRootModificationUtil;
import com.intellij.openapi.vfs.VirtualFile;

import java.io.File;
import java.util.Collections;

import static org.intellij.erlang.compilation.ErlangModuleTextGenerator.*;

public class ErlangModuleCompilationTest extends ErlangCompilationTestBase {
  public void testBuildSingleFile() throws Exception {
    addSourceFile(myModule, "module1.erl", module("module1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
  }

  public void testBuildWithBehaviourInSingleModule() throws Exception {
    BehaviourBuilder behaviour = behaviour("behaviour1").callback("foo", 0);
    addSourceFile(myModule, "behaviour1.erl", behaviour.build());
    addSourceFile(myModule, "module1.erl", module("module1").behaviour(behaviour).build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
  }

  public void testBuildWithParseTransformInDifferentModule() throws Exception {
    Module otherModule = createModuleInOwnDirectoryWithSourceAndTestRoot("other");
    ModuleRootModificationUtil.addDependency(myModule, otherModule);
    addSourceFile(otherModule, "parse_transform1.erl", pt("parse_transform1").build());
    addSourceFile(myModule, "module1.erl", module("module1").pt("parse_transform1").build());
    compileAndAssertOutput(myModule, otherModule);
  }

  public void testBuildWithParseTransformInSingleModule() throws Exception {
    addSourceFile(myModule, "parse_transform1.erl", pt("parse_transform1").build());
    addSourceFile(myModule, "module1.erl", module("module1").pt("parse_transform1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
  }

  public void testRebuildWithNewFile() throws Exception {
    final VirtualFile sourceFile = addSourceFile(myModule, "module1.erl", module("module1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long modificationTime = lastOutputModificationTime(myModule, sourceFile);
    addSourceFile(myModule, "module2.erl", module("module2").build());
    myCompilationRunner.compile();
    assertEquals(modificationTime, lastOutputModificationTime(myModule, sourceFile));
  }

  public void testRebuildWithModificationWithoutDependencies() throws Exception {
    final VirtualFile sourceFile1 = addSourceFile(myModule, "module1.erl", module("module1").build());
    VirtualFile sourceFile2 = addSourceFile(myModule, "module2.erl", module("module2").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long lastModificationTime1 = lastOutputModificationTime(myModule, sourceFile1);
    long lastModificationTime2 = lastOutputModificationTime(myModule, sourceFile2);
    myCompilationRunner.touch(sourceFile2);
    myCompilationRunner.compile();
    assertEquals(lastModificationTime1, lastOutputModificationTime(myModule, sourceFile1));
    assertTrue(lastModificationTime2 != lastOutputModificationTime(myModule, sourceFile2));
  }

  public void testRebuildWithModificationParseTransform() throws Exception {
    VirtualFile parseTransformSourceFile = addSourceFile(myModule, "parse_transform1.erl", pt("parse_transform1").build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").pt("parse_transform1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long parseTransformModificationTime = lastOutputModificationTime(myModule, parseTransformSourceFile);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(parseTransformSourceFile);
    myCompilationRunner.compile();
    assertTrue(parseTransformModificationTime != lastOutputModificationTime(myModule, parseTransformSourceFile));
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }

  public void testRebuildWithModificationBehaviour() throws Exception {
    BehaviourBuilder behaviour = behaviour("behaviour1").callback("foo", 0);
    VirtualFile behaviourSourceFile = addSourceFile(myModule, "behaviour1.erl", behaviour.build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").behaviour(behaviour).build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long behaviourModificationTime = lastOutputModificationTime(myModule, behaviourSourceFile);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(behaviourSourceFile);
    myCompilationRunner.compile();
    assertTrue(behaviourModificationTime != lastOutputModificationTime(myModule, behaviourSourceFile));
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }

  public void testRebuildWithModificationBehaviourAndUnchangeableParseTransform() throws Exception {
    BehaviourBuilder behaviour = behaviour("behaviour1").callback("foo", 0);
    VirtualFile behaviourSourceFile = addSourceFile(myModule, "behaviour1.erl", behaviour.build());
    VirtualFile parseTransformSourceFile = addSourceFile(myModule, "parse_transform1.erl", pt("parse_transform1").build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").behaviour(behaviour).pt("parse_transform1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long behaviourModificationTime = lastOutputModificationTime(myModule, behaviourSourceFile);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    long parseTransformModificationTime = lastOutputModificationTime(myModule, parseTransformSourceFile);
    myCompilationRunner.touch(behaviourSourceFile);
    myCompilationRunner.compile();
    assertEquals(parseTransformModificationTime, lastOutputModificationTime(myModule, parseTransformSourceFile));
    assertTrue(behaviourModificationTime != lastOutputModificationTime(myModule, behaviourSourceFile));
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }

  public void testBuildWithIncludes() throws Exception {
    addSourceFile(myModule, "header.hrl", "");
    addSourceFile(myModule, "module2.erl", module("module2").include("header.hrl").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
  }

  public void testRebuildWithInclude() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").include("header.hrl").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(headerFile);
    myCompilationRunner.compile();
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }

  public void testRebuildWithIncludesDirectory() throws Exception {
    VirtualFile includeSourceRoot = addIncludeRoot(myModule, "include");
    VirtualFile headerFile = addFileToDirectory(includeSourceRoot, "header.hrl", "");
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").include("header.hrl").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(headerFile);
    myCompilationRunner.compile();
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }
  public void testRebuildWithTransitiveDependencies() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    BehaviourBuilder behaviour = behaviour("behaviour1").callback("foo", 0);
    addSourceFile(myModule, "behaviour1.erl", behaviour.include("header.hrl").build());
    VirtualFile sourceFileWithDependency = addSourceFile(myModule, "module1.erl", module("module1").behaviour(behaviour).build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFileWithDependency);
    myCompilationRunner.touch(headerFile);
    myCompilationRunner.compile();
    assertTrue(sourceModificationTime != lastOutputModificationTime(myModule, sourceFileWithDependency));
  }

  public void testBuildWithTestSource() throws Exception {
    addSourceFile(myModule, "module1.erl", module("module1").build());
    addTestFile(myModule, "test1.erl", module("test1").build());
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, true);
    assertSourcesCompiled(myModule, false);
  }

  public void testBuildWithGlobalParseTransform() throws Exception {
    addSourceFile(myModule, "module1.erl", module("module1").build());
    Module otherModule = createModuleInOwnDirectoryWithSourceAndTestRoot("other");
    addSourceFile(otherModule, "parse_transform1.erl", pt("parse_transform1").build());
    addGlobalParseTransform(myModule, Collections.singleton("parse_transform1"));
    ModuleRootModificationUtil.addDependency(myModule, otherModule);
    compileAndAssertOutput(myModule, otherModule);
  }

  private void compileAndAssertOutput(Module... modules) throws Exception {
    CompilationRunner compilationRunner = new CompilationRunner(modules);
    try {
      compilationRunner.compile();
      for (Module module : modules) {
        assertSourcesCompiled(module, false);
      }
    }
    finally {
      compilationRunner.tearDown();
    }
  }

  private static long lastOutputModificationTime(Module module, VirtualFile sourceFile) {
    File outputFile = getOutputFile(module, sourceFile, false);
    assertNotNull(outputFile);
    return outputFile.lastModified();
  }
}
