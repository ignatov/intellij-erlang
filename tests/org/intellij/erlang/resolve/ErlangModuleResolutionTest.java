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

package org.intellij.erlang.resolve;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangModuleResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/resolve/module/" + getTestName(true) + "/";
  }

  protected void doTest(String expectedModulePath, String ... filePaths) {
    myFixture.configureByFiles(filePaths);
    PsiElement focusedElement = myFixture.getFile().findElementAt(myFixture.getEditor().getCaretModel().getOffset());
    focusedElement = PsiTreeUtil.getParentOfType(focusedElement, ErlangModuleRef.class);
    assertNotNull(focusedElement);
    PsiReference reference = focusedElement.getReference();
    assertNotNull(reference);
    PsiElement moduleElement = reference.resolve();
    assertNotNull(moduleElement);
    PsiFile containingFile = moduleElement.getContainingFile();
    assertNotNull(containingFile);
    VirtualFile virtualFile = containingFile.getVirtualFile();
    assertNotNull(virtualFile);
    String actualModulePath = virtualFile.getPath();
    assertTrue(actualModulePath.endsWith(expectedModulePath));
  }

  public void testKernelModulesAppearFirst() {
    doTest("kernel-2.15.2/src/file.erl", "src/test.erl", "src/file.erl");
  }

  public void testStdlibModulesAppearFirst() {
    doTest("stdlib-1.18.2/src/io.erl", "src/test.erl", "src/io.erl");
  }

  //TODO add small IDE tests
  //TODO test source roots are preferred to plain directories
  //TODO test plain directories are preferred to hidden directories
}
