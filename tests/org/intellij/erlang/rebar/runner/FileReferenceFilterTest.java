/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.filters.Filter;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestCase;

import java.io.File;

@SuppressWarnings("ConstantConditions")
public class FileReferenceFilterTest extends PlatformTestCase {

  public void setUp() throws Exception {
    super.setUp();
    final File currentTestRoot = new File("testData/rebar/sampleProject");
    FileUtil.copyDir(currentTestRoot, new File(getProject().getBaseDir().getPath()));
  }

  public void testCompilationErrorRelativePath() {
    final FileReferenceFilter compilationErrorFilter =
      new FileReferenceFilter(getProject(), RebarRunningState.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||src/a_module.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(31, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testCompilationErrorAbsolutePath() {
    final FileReferenceFilter compilationErrorFilter =
      new FileReferenceFilter(getProject(), RebarRunningState.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||" + getProject().getBasePath() + "/src/a_module.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(32 + getProject().getBasePath().length(), result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testCompilationErrorMissingPath() {
    final FileReferenceFilter compilationErrorFilter =
      new FileReferenceFilter(getProject(), RebarRunningState.COMPILATION_ERROR_PATH);
    final String consoleOutput = "some text||src/A_module.erl:123: more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(11, result.highlightStartOffset);
    assertEquals(31, result.highlightEndOffset);
    assertNull(result.hyperlinkInfo);
  }

  public void testEunitErrorPath() {
    final FileReferenceFilter compilationErrorFilter =
      new FileReferenceFilter(getProject(), RebarRunningState.EUNIT_ERROR_PATH);
    final String consoleOutput = "some text (src/a_module.erl, line 123) more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(10, result.highlightStartOffset);
    assertEquals(38, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }

  public void testEunitFailurePath() {
    final FileReferenceFilter compilationErrorFilter =
      new FileReferenceFilter(getProject(), RebarRunningState.EUNIT_FAILURE_PATH);
    final String consoleOutput = "some text [{file,\"src/a_module.erl\"},{line,123}] more text here";
    final Filter.Result result = compilationErrorFilter.applyFilter(consoleOutput, consoleOutput.length());
    assertEquals(10, result.highlightStartOffset);
    assertEquals(48, result.highlightEndOffset);
    assertNotNull(result.hyperlinkInfo);
  }
}
