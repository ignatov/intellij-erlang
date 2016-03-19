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
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangModuleResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
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

  public void testKernelModulesAppearFirst() { doModuleRefTest("kernel-2.15.2/src/file.erl", "test.erl", "file.erl"); }
  public void testStdlibModulesAppearFirst() { doModuleRefTest("stdlib-1.18.2/src/io.erl", "test.erl", "io.erl"); }
  public void testUserModulesAppearFirst()   { doModuleRefTest("eunit.erl", "test.erl", "eunit.erl"); }

  public void testModuleVariableInSpec() { doParameterTest("variableInArguments.erl", "variableInArguments.erl"); }
  public void testModuleTypeInSpec()     { doParameterTest("typeInArguments.erl", "typeInArguments.erl"); }


  private void doModuleRefTest(String expectedPath, String ... filePaths) {
    doTest(ErlangModuleRef.class, expectedPath, filePaths);
  }

  private void doParameterTest(String expectedPath, String ... filePaths) {
    doTest(ErlangQAtom.class, expectedPath, filePaths);
  }

  private void doTest(@NotNull Class<? extends PsiElement> clazz, String expectedPath, String... filePaths) {
    myFixture.configureByFiles(filePaths);
    PsiElement focusedElement = getElementAtCaret(clazz);
    PsiReference reference = focusedElement.getReference();
    PsiElement module = reference != null ? reference.resolve() : null;
    PsiFile containingFile = module != null ? module.getContainingFile() : null;
    VirtualFile virtualFile = containingFile != null ? containingFile.getVirtualFile() : null;
    assertNotNull(virtualFile);
    String actualModulePath = virtualFile.getPath();
    assertTrue("Expected path: *" + expectedPath + ", Actual: " + actualModulePath,
               actualModulePath.endsWith(expectedPath));
  }
  //TODO add small IDE tests
  //TODO test source roots are preferred to plain directories
  //TODO test plain directories are preferred to hidden directories
}
