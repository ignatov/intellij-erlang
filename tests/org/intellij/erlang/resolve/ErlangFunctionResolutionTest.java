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
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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

  public void testGetArityFromParameter()      { doParameterTest("test.erl", "test.erl", "bar", 1); }
  public void testGetModuleFromParameter()     { doParameterTest("test.erl", "module.erl", "bar", 1); }
  public void testFunParameterInSpawnMonitor() { doParameterTest("test.erl", "test.erl", "foo", 1); }

  private void doFunctionCallTest(@NotNull String focusedFile, @NotNull String expectedFile,
                                  @NotNull String... otherFiles) {
    String[] files = {focusedFile, expectedFile};
    myFixture.configureByFiles(ArrayUtil.mergeArrays(files, otherFiles));

    ErlangFunctionCallExpression functionCall = getElementAtCaret(ErlangFunctionCallExpression.class);
    PsiReference reference = functionCall.getReference();
    String name = functionCall.getName();
    int arity = functionCall.getArgumentList().getExpressionList().size();

    assertResolvesTo(reference, expectedFile, name, arity);
  }

  private void doParameterTest(@NotNull String focusedFile,
                               @NotNull String expectedFile,
                               @NotNull String expectedFunction,
                               int expectedArity) {
    myFixture.configureByFiles(focusedFile, expectedFile);

    ErlangQAtom focusedAtom = getElementAtCaret(ErlangQAtom.class);
    PsiReference reference = focusedAtom.getReference();

    assertResolvesTo(reference, expectedFile, expectedFunction, expectedArity);
  }

  private static void assertResolvesTo(@Nullable PsiReference reference,
                                       @NotNull String file,
                                       @NotNull String function,
                                       int arity) {
    assertNotNull(reference);

    ErlangFunction resolvedFunction = ObjectUtils.tryCast(reference.resolve(), ErlangFunction.class);
    assertNotNull(resolvedFunction);

    String actualFile = resolvedFunction.getContainingFile().getName();
    assertEquals(file, actualFile);

    assertEquals(function, resolvedFunction.getName());
    assertEquals(arity, resolvedFunction.getArity());
  }
}
