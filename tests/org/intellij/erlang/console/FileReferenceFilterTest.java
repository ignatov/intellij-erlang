/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.console;

import com.intellij.execution.filters.Filter;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.regex.Pattern;

@SuppressWarnings("ConstantConditions")
public class FileReferenceFilterTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  public void setUp() throws Exception {
    super.setUp();
    final File currentTestRoot = new File("testData/rebar/sampleProject");
    FileUtil.copyDir(currentTestRoot, new File(getProject().getBaseDir().getPath()));
    setUpProjectSdk();
  }

  public void testCompilationErrorRelativePath() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||src/a_module.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(31, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testCompilationErrorAbsolutePath() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||" + getProject().getBasePath() + "/src/a_module.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(32 + getProject().getBasePath().length(), result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testCompilationErrorMissingPath() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||src/B_module.erl:123: more text here"; // may be case insensitive
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(31, result.highlightEndOffset);
    assertNull(result.hyperlinkInfo);
  }

  public void testEunitErrorPath() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH);
    final String consoleOutput = "some text (src/a_module.erl, line 123) more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(37, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }
  
  public void testEunitFullErrorPath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH);
    File base = new File(getProject().getBaseDir().getPath());
    File file = ContainerUtil.getFirstItem(FileUtil.findFilesByMask(Pattern.compile(".*\\.erl"), base));
    VirtualFile vFile = LocalFileSystem.getInstance().findFileByIoFile(file);
    assertNotNull(vFile);
    String canonicalPath = vFile.getCanonicalPath();
    final String consoleOutput = "in function bisect_server_test:'-false_test/0-fun-0-'/0 (" + canonicalPath + ", line 14)";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.hyperlinkInfo);
  }

  public void testEunitFailurePath() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_FAILURE_PATH);
    final String consoleOutput = "some text [{file,\"src/a_module.erl\"},{line,123}] more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(10, result.highlightStartOffset);
    assertEquals(48, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testLinkToSdkFile() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||src/lists.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.hyperlinkInfo);
  }

  public void testLinkToSdkFileNoSrc() {
    final FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||lists.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.hyperlinkInfo);
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
      }
    };
  }
}
