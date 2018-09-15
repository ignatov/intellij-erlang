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

package org.intellij.erlang.eunit;

import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class ErlangUnitTestElementUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/eunit/config/";
  }

  public void testFunctionSelection() {
    myFixture.configureByFiles("tests1.erl", "tests2.erl");
    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(myFixture.getElementAtCaret());

    assertNotNull(functions);
    assertEquals(1, functions.size());
    assertEquals(myFixture.getElementAtCaret(), functions.iterator().next());
  }

  public void testSingleFileSelection() {
    myFixture.configureByFiles("tests1.erl", "tests2.erl");
    MyMockDataContext dataContext = new MyMockDataContext(myFixture.getFile().getVirtualFile());
    Collection<ErlangFile> files = ErlangUnitTestElementUtil.findFileTestElements(myFixture.getProject(), dataContext);

    assertNotNull(files);
    assertEquals(1, files.size());
    assertEquals(myFixture.getFile(), files.iterator().next());
  }

  public void testMultipleFilesSelection() {
    PsiFile[] psiFiles = myFixture.configureByFiles("tests1.erl", "tests2.erl");
    MyMockDataContext dataContext = new MyMockDataContext(getVirtualFiles(psiFiles));
    Collection<ErlangFile> files = ErlangUnitTestElementUtil.findFileTestElements(myFixture.getProject(), dataContext);

    assertNotNull(files);
    assertEquals(psiFiles.length, files.size());
    assertContainsElements(files, psiFiles);
  }

  public void testDirectorySelection() {
    PsiFile[] psiFiles = myFixture.configureByFiles("tests1.erl", "tests2.erl");
    @SuppressWarnings("ConstantConditions") MyMockDataContext dataContext =
      new MyMockDataContext(psiFiles[0].getParent().getVirtualFile());
    Collection<ErlangFile> files = ErlangUnitTestElementUtil.findFileTestElements(myFixture.getProject(), dataContext);

    assertNotNull(files);
    assertEquals(psiFiles.length, files.size());
    assertContainsElements(files, psiFiles);
  }

  private static VirtualFile[] getVirtualFiles(PsiFile[] psiFiles) {
    VirtualFile[] virtualFiles = new VirtualFile[psiFiles.length];
    for (int i = 0; i < psiFiles.length; i++) {
      virtualFiles[i] = psiFiles[i].getVirtualFile();
    }
    return virtualFiles;
  }

  private static class MyMockDataContext implements DataContext {
    private final VirtualFile[] myVirtualFiles;

    private MyMockDataContext(VirtualFile ... virtualFiles) {
      myVirtualFiles = virtualFiles;
    }

    @Nullable
    @Override
    public Object getData(@NonNls String dataId) {
      if (CommonDataKeys.VIRTUAL_FILE_ARRAY.getName().equals(dataId)) {
        return myVirtualFiles;
      }
      return null;
    }
  }
}
