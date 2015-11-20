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

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleRootModificationUtil;
import com.intellij.openapi.vfs.VirtualFile;

import java.util.Collections;

import static org.junit.Assert.assertNotEquals;

public class ErlangBuildInDifferentModuleTest extends ErlangCompilationTestBase {

  private Module myOtherModule;
  private CompilationRunner myCompilationRunner;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myOtherModule = createModuleInOwnDirectoryWithSourceAndTestRoot("other");
    ModuleRootModificationUtil.addDependency(myModule, myOtherModule);
    myCompilationRunner = new CompilationRunner(myModule, myOtherModule);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      myCompilationRunner.tearDown();
    }
    finally {
      super.tearDown();
    }
  }

  public void testBuildWithGlobalParseTransform() throws Exception {
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").build());
    addSourceFile(myOtherModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    addGlobalParseTransform(myModule, Collections.singleton("parse_transform1"));
    compileAndAssertOutput();
  }

  public void testBuildWithParseTransformInDifferentModule() throws Exception {
    addSourceFile(myOtherModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").pt("parse_transform1").build());
    compileAndAssertOutput();
  }

  public void testBuildWithBehaviourInDifferentModule() throws Exception {
    ErlangModuleTextGenerator.BehaviourBuilder behaviour = ErlangModuleTextGenerator.behaviour("behaviour1").callback("foo", 0);
    addSourceFile(myOtherModule, "behaviour1.erl", behaviour.build());
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").behaviour(behaviour).build());
    compileAndAssertOutput();
  }

  public void testBuildWithIncludesFormDifferentModule() throws Exception {
    VirtualFile includeSourceRoot = addIncludeRoot(myOtherModule, "include");
    addFileToDirectory(includeSourceRoot, "header.hrl", "");
    addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").include("../other/include/header.hrl").build());
    compileAndAssertOutput();
  }

  public void testRebuildWithParseTransformInDifferentModule() throws Exception {
    VirtualFile parseTransform = addSourceFile(myOtherModule, "parse_transform1.erl", ErlangModuleTextGenerator.pt("parse_transform1").build());
    VirtualFile sourceFile = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").pt("parse_transform1").build());
    doTestCrossModulesDependency(parseTransform, sourceFile);
  }

  public void testRebuildWithBehaviourInDifferentModule() throws Exception {
    ErlangModuleTextGenerator.BehaviourBuilder behaviour = ErlangModuleTextGenerator.behaviour("behaviour1").callback("foo", 0);
    VirtualFile behaviourFile = addSourceFile(myOtherModule, "behaviour1.erl", behaviour.build());
    VirtualFile sourceFile = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").behaviour(behaviour).build());
    doTestCrossModulesDependency(behaviourFile, sourceFile);
  }

  public void testRebuildWithIncludesInDifferentModule() throws Exception {
    VirtualFile headerFile = addSourceFile(myModule, "header.hrl", "");
    VirtualFile sourceFile = addSourceFile(myModule, "module1.erl", ErlangModuleTextGenerator.module("module1").include("header.hrl").build());
    doTestCrossModulesDependency(headerFile, sourceFile);
  }

  protected void compileAndAssertOutput() throws Exception {
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    assertSourcesCompiled(myOtherModule, false);
  }

  protected void doTestCrossModulesDependency(VirtualFile dependency,
                                              VirtualFile sourceFile) throws Exception {
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    assertSourcesCompiled(myOtherModule, false);
    long sourceModificationTime = lastOutputModificationTime(myModule, sourceFile);
    myCompilationRunner.touch(dependency);
    myCompilationRunner.compile();
    assertSourcesCompiled(myModule, false);
    assertSourcesCompiled(myOtherModule, false);
    assertNotEquals(sourceFile.getPath() + " wasn't rebuilt after change in dependency.",
                    sourceModificationTime,
                    lastOutputModificationTime(myModule, sourceFile));
  }
}
