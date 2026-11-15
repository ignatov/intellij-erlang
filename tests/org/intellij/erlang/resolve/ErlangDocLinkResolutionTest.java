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

package org.intellij.erlang.resolve;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangDocLinkResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String HANDLER = """
    -module(handler).
    -export([system_cmd/5]).
    system_cmd(_, _, _, _, _) -> ok.
    system_cmd(_) -> ok.
    """;

  public void testSeeRemoteFunction() {
    doFunctionTest("%% @see handler:system_<caret>cmd/5", "handler.erl", "system_cmd", 5);
  }

  public void testSeeModuleOfRemoteFunction() {
    doModuleTest("%% @see hand<caret>ler:system_cmd/5", "handler");
  }

  public void testSeeModule() {
    doModuleTest("%% @see hand<caret>ler", "handler");
  }

  public void testSeeModuleInModuleDocComment() {
    doModuleTest("%%% @see hand<caret>ler", "handler");
  }

  public void testSeeLocalFunction() {
    doFunctionTest("%% @see fo<caret>o/1", "test.erl", "foo", 1);
  }

  public void testSeeApplicationModule() {
    doModuleTest("%% @see //app/hand<caret>ler", "handler");
  }

  public void testSeeApplicationModuleFunction() {
    doFunctionTest("%% @see //app/handler:system_<caret>cmd/5", "handler.erl", "system_cmd", 5);
  }

  public void testLinkFunction() {
    doFunctionTest("%% Runs {@link handler:system_<caret>cmd/5} first.", "handler.erl", "system_cmd", 5);
  }

  public void testLinkLocalFunctionWithDescription() {
    doFunctionTest("%% See {@link fo<caret>o/0. the other one}.", "test.erl", "foo", 0);
  }

  public void testSeeFunctionFollowedByDescription() {
    doFunctionTest("%% @see fo<caret>o/1. More text", "test.erl", "foo", 1);
  }

  public void testSeeModuleFollowedByPeriod() {
    doModuleTest("%% @see hand<caret>ler.", "handler");
  }

  public void testSeeFunctionWithSpacedArity() {
    doFunctionTest("%% @see fo<caret>o / 1", "test.erl", "foo", 1);
  }

  public void testNoReferenceForLinkWithoutSeparator() {
    assertNull(configure("%% See {@link fo<caret>o/0 the other one}."));
  }

  public void testNoReferenceForSeeInsideText() {
    assertNull(configure("%% @doc Not a tag: @see fo<caret>o/1"));
  }

  public void testSeeFunctionWithWrongArity() {
    PsiReference reference = configure("%% @see handler:system_<caret>cmd/2");
    assertNotNull(reference);
    assertNull(reference.resolve());
  }

  public void testNoReferenceOutsideTags() {
    assertNull(configure("%% Calls handler:system_<caret>cmd/5."));
  }

  public void testNoReferenceInPlainComment() {
    assertNull(configure("% @see handler:system_<caret>cmd/5"));
  }

  public void testRenameLocalFunctionUpdatesSee() {
    myFixture.configureByText("test.erl", "-module(test).\n%% @see fo<caret>o/1\nfoo(_) -> foo(1).\n");
    myFixture.renameElement(myFixture.getElementAtCaret(), "bar");
    myFixture.checkResult("-module(test).\n%% @see bar/1\nbar(_) -> bar(1).\n");
  }

  public void testRenameToQuotedNameKeepsSee() {
    myFixture.configureByText("test.erl", "-module(test).\n%% @see fo<caret>o/1\nfoo(_) -> foo(1).\n");
    myFixture.renameElement(myFixture.getElementAtCaret(), "Bar");
    myFixture.checkResult("-module(test).\n%% @see foo/1\n'Bar'(_) -> 'Bar'(1).\n");
  }

  public void testRenameModuleFileWithSeveralLinksInOneComment() {
    PsiFile handler = myFixture.addFileToProject("handler.erl", "-module(handler).\n-export([a/1]).\na(_) -> ok.\n");
    PsiFile user = myFixture.addFileToProject("user1.erl", "-module(user1).\n%% {@link handler:a/1} or {@link handler}\nx() -> handler:a(1).\n");
    myFixture.renameElement(handler, "h2.erl");
    assertEquals("h2.erl", handler.getName());
    assertEquals("-module(h2).\n-export([a/1]).\na(_) -> ok.\n", handler.getText());
    assertEquals("-module(user1).\n%% {@link h2:a/1} or {@link h2}\nx() -> h2:a(1).\n", user.getText());
  }

  public void testSeeDoesNotMakeFunctionUsed() {
    myFixture.enableInspections(ErlangUnusedFunctionInspection.class);
    myFixture.configureByText("test.erl", """
      -module(test).
      -export([foo/0]).
      %% @see bar/1
      foo() -> ok.
      <warning descr="Unused function 'bar/1'">bar</warning>(_) -> ok.
      """);
    myFixture.checkHighlighting(true, false, false);
  }

  private void doFunctionTest(@NotNull String comment, @NotNull String expectedFile, @NotNull String name, int arity) {
    PsiReference reference = configure(comment);
    assertNotNull(reference);
    assertTrue(reference.isSoft());
    PsiElement resolved = reference.resolve();
    assertInstanceOf(resolved, ErlangFunction.class);
    assertEquals(name, ((ErlangFunction) resolved).getName());
    assertEquals(arity, ((ErlangFunction) resolved).getArity());
    assertEquals(expectedFile, resolved.getContainingFile().getName());
    assertEquals(resolved, myFixture.getElementAtCaret());
  }

  private void doModuleTest(@NotNull String comment, @NotNull String expectedModule) {
    PsiReference reference = configure(comment);
    assertNotNull(reference);
    PsiElement resolved = reference.resolve();
    assertInstanceOf(resolved, ErlangModule.class);
    assertEquals(expectedModule, ErlangPsiImplUtil.getName((ErlangModule) resolved));
    assertEquals(resolved, myFixture.getElementAtCaret());
  }

  private PsiReference configure(@NotNull String comment) {
    myFixture.addFileToProject("handler.erl", HANDLER);
    myFixture.configureByText("test.erl", "-module(test).\n" +
                                          comment + "\n" +
                                          "foo() -> ok.\n" +
                                          "foo(_) -> ok.\n");
    return myFixture.getFile().findReferenceAt(myFixture.getCaretOffset());
  }
}
