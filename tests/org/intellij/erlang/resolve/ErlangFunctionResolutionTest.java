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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangFunctionResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
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
    return "testData/resolve/function/" + getTestName(true) + "/";
  }

  public void testNoAutoImport()               { doFunctionCallTest("test.erl", "incl.erl"); }
  public void testNoAutoImportWithTuple()      { doFunctionCallTest("test.erl", "incl.erl"); }
  public void testPreferImportFromUserModule() { doFunctionCallTest("test.erl", "incl.erl"); }
  public void testPreferLocalFunction()        { doFunctionCallTest("test.erl", "test.erl", "incl.erl"); }
  public void testPreferFirstImport()          { doFunctionCallTest("test.erl", "incl.erl", "another_incl.erl"); }

  public void testGetArityFromParameter()      { doParameterTest("test.erl", "test.erl"); }
  public void testGetModuleFromParameter()     { doParameterTest("test.erl", "module.erl"); }
  public void testFunParameterInSpawnMonitor() { doParameterTest("test.erl", "test.erl"); }

  private void doFunctionCallTest(@NotNull String focusedFilePath, @NotNull String expectedFilePath,
                                  @NotNull String... otherFilePaths) {
    String[] paths = {focusedFilePath, expectedFilePath};
    PsiFile[] files = myFixture.configureByFiles(ArrayUtil.mergeArrays(paths, otherFilePaths));
    ErlangFile focusedFile = (ErlangFile) files[0];
    ErlangFile expectedFile = (ErlangFile) files[1];
    int offset = myFixture.getEditor().getCaretModel().getOffset();

    ErlangFunctionCallExpression functionCall = PsiTreeUtil.getParentOfType(
      focusedFile != null ? focusedFile.findElementAt(offset) : null, ErlangFunctionCallExpression.class);
    assertNotNull(functionCall);
    ErlangFunction resolvedFunction = ErlangPsiImplUtil.resolveToFunction(functionCall);

    String name = functionCall.getName();
    int arity = functionCall.getArgumentList().getExpressionList().size();
    assertSame(expectedFile != null ? expectedFile.getFunction(name, arity) : null, resolvedFunction);
  }

  private void doParameterTest(@NotNull String focusedFilePath, @NotNull String expectedFilePath) {
    PsiFile[] files = myFixture.configureByFiles(focusedFilePath, expectedFilePath);
    ErlangFile focusedFile = (ErlangFile) files[0];
    ErlangFile expectedFile = (ErlangFile) files[1];
    assertTrue(focusedFile != null && expectedFile != null);
    int offset = myFixture.getEditor().getCaretModel().getOffset();

    ErlangQAtom focusedAtom = PsiTreeUtil.getParentOfType(focusedFile.findElementAt(offset), ErlangQAtom.class);
    PsiReference reference = focusedAtom != null ? focusedAtom.getReference() : null;
    PsiElement resolvedFunction = reference != null ? reference.resolve() : null;
    assertNotNull(resolvedFunction);

    ErlangFunctionReferenceImpl expectedReference = ErlangPsiImplUtil.createFunctionReference(focusedAtom);
    assertSame(expectedFile.getFunction(expectedReference.getName(), expectedReference.getArity()), resolvedFunction);
  }
}
