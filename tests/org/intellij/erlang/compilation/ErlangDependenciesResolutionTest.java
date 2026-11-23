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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.util.ThrowableComputable;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.JavaModuleTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.builder.ErlangFileDescriptor;
import org.intellij.erlang.module.ErlangModuleType;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ErlangDependenciesResolutionTest extends JavaModuleTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpModule();
    String sourceDirectoryPath = getSourceDirectoryPath();
    if (directoryExists(sourceDirectoryPath)) {
      setUpSourcePath(sourceDirectoryPath);
    }
    String testsDirectoryPath = getTestsDirectoryPath();
    if (directoryExists(testsDirectoryPath)) {
      setUpTestsPath(testsDirectoryPath);
    }
  }

  @Override
  protected ModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  private void setUpSourcePath(String sourcePath) throws IOException {
    doSetUpSourcePath(sourcePath, false);
  }

  private void setUpTestsPath(String sourcePath) throws IOException {
    doSetUpSourcePath(sourcePath, true);
  }

  private void doSetUpSourcePath(final String sourcePath, boolean isTestSource) throws IOException {
    final String sourceDirectoryName = isTestSource ? "test" : "src";
    // The .iml file is not written to disk until the project is saved, so use its directory instead.
    final String moduleDirPath = myModule.getModuleNioFile().getParent().toString();
    VirtualFile moduleSourceDir = ApplicationManager.getApplication().runWriteAction((ThrowableComputable<VirtualFile, IOException>) () -> {
      VirtualFile moduleSourceDir1 = VfsUtil.createDirectoryIfMissing(moduleDirPath + "/" + sourceDirectoryName);
      FileUtil.copyDirContent(new File(sourcePath), new File(moduleSourceDir1.getPath()));
      VfsUtil.markDirtyAndRefresh(false, true, true, moduleSourceDir1);
      return moduleSourceDir1;
    });
    PsiTestUtil.addSourceRoot(myModule, moduleSourceDir, isTestSource);
  }

  private String getSourceDirectoryPath() {
    return getTestDataRoot() + "src";
  }

  private String getTestsDirectoryPath() {
    return getTestDataRoot() + "test";
  }

  private String getTestDataRoot() {
    return "testData/compilation/" + getTestName(false) + "/";
  }

  private static boolean directoryExists(String dirPath) {
    File dir = new File(dirPath);
    return dir.exists() && dir.isDirectory();
  }

  public void testCyclicDependency() {
    try {
      ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
      fail("Expected a cyclic dependency exception to be thrown.");
    }
    catch (ErlangPrepareDependenciesCompileTask.CyclicDependencyFoundException ignored) {
    }
  }

  public void testDependenciesAreCompiledFirst() throws Exception {
    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    assertSameErlangFiles(moduleBuildOrder, "parse_transform1", "parse_transform2", "behaviour1", "module1");
  }

  public void testDependenciesWithIncludes() throws Exception {
    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    assertSameErlangFiles(moduleBuildOrder, "parse_transform1", "header1", "behaviour1", "header2", "module1");
  }

  public void testGuardedMutualIncludes() throws Exception {
    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    List<String> actualModules = ContainerUtil.map(getModulePaths(moduleBuildOrder), path -> FileUtil.getNameWithoutExtension(new File(path)));
    assertContainsElements(actualModules, "m");
  }

  public void testGuardedMutualIncludesWithParseTransform() throws Exception {
    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    List<String> erlangModules = ContainerUtil.mapNotNull(getModulePaths(moduleBuildOrder),
                                                          path -> path.endsWith(".erl") ? FileUtil.getNameWithoutExtension(new File(path)) : null);
    assertOrderedEquals(erlangModules, "pt", "m");
  }

  public void testMutualIncludesThroughModule() {
    try {
      ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
      fail("Expected a cyclic dependency exception to be thrown.");
    }
    catch (ErlangPrepareDependenciesCompileTask.CyclicDependencyFoundException ignored) {
    }
  }

  public void testTestsDependency() throws Exception {
    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    assertSameErlangFiles(moduleBuildOrder, "src_parse_transform", "test_parse_transform", "test");
  }

  public void testLargeModuleDoesNotBreakBuildOrder() throws Exception {
    // A module above the code assistance limit (idea.max.intellisense.filesize) gets no Erlang PSI, but erlc compiles it.
    String moduleSourceDirPath = myModule.getModuleNioFile().getParent().toString() + "/src";
    StringBuilder text = new StringBuilder("-module(big).\n-export([f/0]).\n\nf() -> ok.\n");
    String commentLine = "%" + "x".repeat(99) + "\n";
    while (text.length() < 3_000_000) text.append(commentLine);
    File bigFile = new File(moduleSourceDirPath, "big.erl");
    FileUtil.writeToFile(bigFile, text.toString());
    VfsUtil.markDirtyAndRefresh(false, true, true, new File(moduleSourceDirPath));

    List<ErlangFileDescriptor> moduleBuildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    List<String> actualModules = ContainerUtil.map(getModulePaths(moduleBuildOrder), path -> FileUtil.getNameWithoutExtension(new File(path)));
    assertSameElements(actualModules, "big", "module1");
  }

  public void testLargeModuleDependsOnAllHeaders() throws Exception {
    writeAboveSizeLimit("big.erl", "-module(big).\n-include(\"header1.hrl\").\n");
    Map<String, List<String>> dependencies = getDependencies(ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule));
    assertSameElements(dependencies.get("big.erl"), "header1.hrl", "header2.hrl");
    assertEmpty(dependencies.get("module1.erl"));
  }

  public void testLargeHeaderIsDependencyOfAllModules() throws Exception {
    writeAboveSizeLimit("big.hrl", "-include(\"header1.hrl\").\n");
    Map<String, List<String>> dependencies = getDependencies(ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule));
    assertSameElements(dependencies.get("big.hrl"), "header1.hrl");
    assertEmpty(dependencies.get("header1.hrl"));
    assertSameElements(dependencies.get("module1.erl"), "big.hrl");
    assertSameElements(dependencies.get("module2.erl"), "header1.hrl", "big.hrl");
  }

  public void testLargeHeaderWithParseTransformInHeaderIsNotCyclic() throws Exception {
    writeAboveSizeLimit("big.hrl", "-define(X, 1).\n");
    List<ErlangFileDescriptor> buildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    List<String> files = ContainerUtil.map(getModulePaths(buildOrder), path -> new File(path).getName());
    assertTrue(files.toString(), files.indexOf("pt.erl") < files.indexOf("m.erl"));
    Map<String, List<String>> dependencies = getDependencies(buildOrder);
    assertSameElements(dependencies.get("m.erl"), "h.hrl", "big.hrl");
    assertSameElements(dependencies.get("pt.erl"), "big.hrl");
  }

  public void testLargeParseTransformAndBehaviourAreDependencies() throws Exception {
    writeAboveSizeLimit("pt.erl", "-module(pt).\n-export([parse_transform/2]).\n\nparse_transform(Forms, _Options) -> Forms.\n");
    writeAboveSizeLimit("beh.erl", "-module(beh).\n-callback init() -> ok.\n");
    List<ErlangFileDescriptor> buildOrder = ErlangPrepareDependenciesCompileTask.getBuildOrder(myModule);
    List<String> files = ContainerUtil.map(getModulePaths(buildOrder), path -> new File(path).getName());
    Map<String, List<String>> dependencies = getDependencies(buildOrder);
    assertSameElements(dependencies.get("m.erl"), "pt.erl", "beh.erl");
    assertTrue(files.toString(), files.indexOf("pt.erl") < files.indexOf("m.erl"));
    assertTrue(files.toString(), files.indexOf("beh.erl") < files.indexOf("m.erl"));
  }

  private void writeAboveSizeLimit(String fileName, String text) throws IOException {
    // A file above the code assistance limit (idea.max.intellisense.filesize) gets no Erlang PSI, but erlc compiles it.
    String moduleSourceDirPath = myModule.getModuleNioFile().getParent().toString() + "/src";
    StringBuilder builder = new StringBuilder(text);
    String commentLine = "%" + "x".repeat(99) + "\n";
    while (builder.length() < 3_000_000) builder.append(commentLine);
    FileUtil.writeToFile(new File(moduleSourceDirPath, fileName), builder.toString());
    VfsUtil.markDirtyAndRefresh(false, true, true, new File(moduleSourceDirPath));
  }

  @NotNull
  private static Map<String, List<String>> getDependencies(List<ErlangFileDescriptor> buildOrder) {
    Map<String, List<String>> dependencies = new HashMap<>();
    for (ErlangFileDescriptor descriptor : buildOrder) {
      dependencies.put(new File(descriptor.myPath).getName(),
                       ContainerUtil.map(descriptor.myDependencies, path -> new File(path).getName()));
    }
    return dependencies;
  }

  private static void assertSameErlangFiles(List<ErlangFileDescriptor> moduleBuildOrder,
                                            String... expectedModules) {
    List<String> actualModules = ContainerUtil.map(getModulePaths(moduleBuildOrder), path -> FileUtil.getNameWithoutExtension(new File(path)));
    assertOrderedEquals(actualModules, expectedModules);
  }

  @NotNull
  private static List<String> getModulePaths(List<ErlangFileDescriptor> buildOrder) {
    return ContainerUtil.mapNotNull(buildOrder, erlangFileDescriptor -> erlangFileDescriptor.myPath);
  }
}