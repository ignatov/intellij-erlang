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

package org.intellij.erlang.console;

import com.intellij.execution.filters.Filter;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.List;
import java.util.regex.Pattern;

@SuppressWarnings("ConstantConditions")
public class FileReferenceFilterTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void setUp() throws Exception {
    super.setUp();
    File currentTestRoot = new File("testData/rebar/sampleProject");
    String basePath = getProject().getBasePath();
    assertNotNull(basePath);
    FileUtil.copyDir(currentTestRoot, new File(basePath));
    setUpProjectSdk();
  }

  public void testCompilationErrorRelativePath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    String consoleOutput = "some text||src/a_module.erl:123: more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    List<Filter.ResultItem> resultItems = result.getResultItems();
    Filter.ResultItem item = resultItems.get(0);
    assertEquals(11, item.getHighlightStartOffset());
    assertEquals(31, item.getHighlightEndOffset());
    assertNotNull(item.getHyperlinkInfo());
  }

  public void testCompilationErrorAbsolutePath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    String consoleOutput = "some text||" + getProject().getBasePath() + "/src/a_module.erl:123: more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());

    List<Filter.ResultItem> resultItems = result.getResultItems();
    Filter.ResultItem item = resultItems.get(0);
    assertEquals(11, item.getHighlightStartOffset());
    assertEquals(32 + getProject().getBasePath().length(), item.getHighlightEndOffset());
    assertNotNull(item.getHyperlinkInfo());
  }

  public void testCompilationErrorMissingPath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    String consoleOutput = "some text||src/B_module.erl:123: more text here"; // may be case insensitive
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    List<Filter.ResultItem> resultItems = result.getResultItems();
    Filter.ResultItem item = resultItems.get(0);
    assertEquals(11, item.getHighlightStartOffset());
    assertEquals(31, item.getHighlightEndOffset());
    assertNull(item.getHyperlinkInfo());
  }

  public void testEunitErrorPath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH);
    String consoleOutput = "some text (src/a_module.erl, line 123) more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    List<Filter.ResultItem> resultItems = result.getResultItems();
    Filter.ResultItem item = resultItems.get(0);
    assertEquals(11, item.getHighlightStartOffset());
    assertEquals(37, item.getHighlightEndOffset());
    assertNotNull(item.getHyperlinkInfo());
  }
  
  public void testEunitFullErrorPath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_ERROR_PATH);
    File base = new File(getProject().getBasePath());
    File file = ContainerUtil.getFirstItem(FileUtil.findFilesByMask(Pattern.compile(".*\\.erl"), base));
    VirtualFile vFile = LocalFileSystem.getInstance().findFileByIoFile(file);
    assertNotNull(vFile);
    String canonicalPath = vFile.getCanonicalPath();
    String consoleOutput = "in function bisect_server_test:'-false_test/0-fun-0-'/0 (" + canonicalPath + ", line 14)";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.getFirstHyperlinkInfo());
  }

  public void testEunitFailurePath() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.EUNIT_FAILURE_PATH);
    String consoleOutput = "some text [{file,\"src/a_module.erl\"},{line,123}] more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    List<Filter.ResultItem> resultItems = result.getResultItems();
    Filter.ResultItem item = resultItems.get(0);
    assertEquals(10, item.getHighlightStartOffset());
    assertEquals(48, item.getHighlightEndOffset());
    assertNotNull(item.getHyperlinkInfo());
  }

  public void testLinkToSdkFile() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    String consoleOutput = "some text||src/lists.erl:123: more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.getFirstHyperlinkInfo());
  }

  public void testLinkToSdkFileNoSrc() {
    FileReferenceFilter compilationErrorFilter = new FileReferenceFilter(getProject(), ErlangConsoleUtil.COMPILATION_ERROR_PATH);
    String consoleOutput = "some text||lists.erl:123: more text here";
    Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertNotNull(result.getFirstHyperlinkInfo());
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }
}
