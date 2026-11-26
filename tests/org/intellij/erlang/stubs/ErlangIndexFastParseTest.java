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

package org.intellij.erlang.stubs;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.stubs.StubTreeBuilder;
import com.intellij.util.indexing.FileContent;
import com.intellij.util.indexing.FileContentImpl;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

// The index parses a file in the fast mode and parses it again in full only when the fast tree has an error.
public class ErlangIndexFastParseTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String VALID = "-module(valid).\n-export([f/0]).\n-record(r, {a}).\nf() -> #r{a = 1}.\n";

  @Override
  protected String getTestDataPath() {
    return "testData/stubs/";
  }

  public void testValidFileIsParsedOnceInFastMode() throws IOException {
    VirtualFile file = myFixture.getTempDirFixture().createFile("valid.erl", VALID);
    assertEquals(List.of("fast"), parsesForIndex(file));
  }

  public void testBrokenFileIsParsedAgainInFull() throws IOException {
    VirtualFile file = myFixture.copyFileToProject("StubMismatchUnclosedRecord.erl");
    assertEquals(List.of("fast", "full"), parsesForIndex(file));
  }

  public void testEditorParseIsFull() {
    List<String> parses = recordParses("editor.erl");
    PsiFile file = PsiFileFactory.getInstance(getProject()).createFileFromText("editor.erl", ErlangFileType.MODULE, VALID);
    assertNotNull(file.getNode().getFirstChildNode());
    assertEquals(List.of("full"), parses);
  }

  private List<String> parsesForIndex(VirtualFile file) throws IOException {
    FileContent content = FileContentImpl.createByFile(file, getProject());
    List<String> parses = recordParses(file.getName());
    assertNotNull(StubTreeBuilder.buildStubTree(content));
    return parses;
  }

  private List<String> recordParses(String fileName) {
    List<String> parses = Collections.synchronizedList(new ArrayList<>());
    ErlangParserUtil.setParseListener((file, fast) -> {
      if (file != null && fileName.equals(file.getName())) parses.add(fast ? "fast" : "full");
    }, getTestRootDisposable());
    return parses;
  }
}
