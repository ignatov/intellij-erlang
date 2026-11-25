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
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangExportFunction;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangFunctionWithArity;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

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
  public void testImportArityWithDigitSeparators() { doFunctionCallTest("test.erl", "incl.erl"); }
  public void testPreferFirstIncludedHeader() {
    doFunctionCallTest("test.erl", "a.hrl", "b.hrl", "c.hrl", "d.hrl", "e.hrl", "g.hrl", "h.hrl", "i.hrl");
  }
  public void testPreferNestedIncludeInSourceOrder() {
    doFunctionCallTest("test.erl", "a.hrl", "outer.hrl", "b.hrl", "c.hrl", "d.hrl", "e.hrl", "g.hrl", "h.hrl", "i.hrl");
  }

  public void testGetArityFromParameter()      { doParameterTest("test.erl", "test.erl", "bar", 1); }
  public void testGetModuleFromParameter()     { doParameterTest("test.erl", "module.erl", "bar", 1); }
  public void testFunParameterInSpawnMonitor() { doParameterTest("test.erl", "test.erl", "foo", 1); }

  public void testExportOfImportedFunction()      { doUnresolvedExportTest("test.erl", "incl.erl"); }
  public void testExportOfAutoImportedBif()       { doUnresolvedExportTest("test.erl"); }
  public void testExportOfLocalImportedFunction() { doExportTest("test.erl", "test.erl", "incl.erl"); }
  public void testExportOfIncludedFunction()      { doExportTest("test.erl", "incl.hrl"); }

  public void testExportArityWithDigitSeparators() {
    myFixture.configureByText("test.erl", """
      -export([<caret>g/1_0]).
      g(_, _, _, _, _, _, _, _, _, _) -> ok.
      g() -> ok.
      """);
    assertResolvesTo(getElementAtCaret(ErlangExportFunction.class).getReference(), "test.erl", "g", 10);
  }

  public void testFunArityWithDigitSeparators() {
    myFixture.configureByText("test.erl", """
      g(_, _, _, _, _, _, _, _, _, _) -> ok.
      g() -> ok.
      h() -> fun <caret>g/1_0.
      """);
    assertResolvesTo(getElementAtCaret(ErlangFunctionWithArity.class).getReference(), "test.erl", "g", 10);
  }

  // issue 1609: the call hello() after setup?PAR, is no function of the source; the export names the real hello/0
  public void testExportOfFunctionAfterSplitForm() {
    myFixture.configureByText("greet.erl", """
      -module(greet).
      -export([start/0, setup/0, <caret>hello/0]).
      -define(PAR, ()).
      start() -> setup?PAR, hello().
      setup() -> ok.
      hello() -> ok.
      """);
    PsiReference reference = getElementAtCaret(ErlangExportFunction.class).getReference();
    assertResolvesTo(reference, "greet.erl", "hello", 0);
    ErlangFunction resolved = (ErlangFunction)reference.resolve();
    assertEquals("hello() -> ok.", resolved.getText());
  }

  // issue 1609: Find Usages of the real hello/0 finds the -export entry
  public void testUsagesOfFunctionAfterSplitForm() {
    myFixture.configureByText("greet.erl", """
      -module(greet).
      -export([start/0, setup/0, hello/0]).
      -define(PAR, ()).
      start() -> setup?PAR, hello().
      setup() -> ok.
      <caret>hello() -> ok.
      """);
    ErlangFunction function = getElementAtCaret(ErlangFunction.class);
    assertEquals("hello() -> ok.", function.getText());
    Collection<UsageInfo> usages = myFixture.findUsages(function);
    assertEquals(1, usages.size());
    assertInstanceOf(ContainerUtil.getFirstItem(usages).getElement(), ErlangExportFunction.class);
  }

  private void doExportTest(@NotNull String focusedFile, @NotNull String expectedFile, @NotNull String... otherFiles) {
    String[] files = {focusedFile, expectedFile};
    myFixture.configureByFiles(ArrayUtil.mergeArrays(files, otherFiles));

    ErlangExportFunction exportFunction = getElementAtCaret(ErlangExportFunction.class);
    String name = ErlangPsiImplUtil.getExportFunctionName(exportFunction);
    int arity = ErlangPsiImplUtil.getArity(exportFunction.getInteger());

    assertResolvesTo(exportFunction.getReference(), expectedFile, name, arity);
  }

  private void doUnresolvedExportTest(@NotNull String... files) {
    myFixture.configureByFiles(files);

    PsiReference reference = getElementAtCaret(ErlangExportFunction.class).getReference();
    assertNull(reference.resolve());
    assertEmpty(((PsiPolyVariantReference) reference).multiResolve(false));
  }

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
