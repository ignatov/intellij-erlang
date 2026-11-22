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

package org.intellij.erlang.info;

import com.intellij.lang.parameterInfo.CreateParameterInfoContext;
import com.intellij.lang.parameterInfo.ParameterInfoHandler;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.utils.parameterInfo.MockParameterInfoUIContext;
import com.intellij.testFramework.utils.parameterInfo.MockUpdateParameterInfoContext;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.intellij.erlang.psi.ErlangArgumentList;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class ErlangParameterInfoHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String FOO = "foo(Arg1, Arg2) -> ok.\n";

  public void testEmpty()     { doTest(FOO + "bar() -> foo(<caret>)", 0); }
  public void testSecond()    { doTest(FOO + "bar() -> foo(1, <caret>)", 1); }
  public void testBif()       { doTest("bar() -> hash({}, <caret>)", 1); }
  public void testModuleBif() { doTest("bar() -> math:sin(<caret>)", 0); }

  public void testMacro() {
    doMacroTest("-define(FOO(A, B), {A, B}).\nbar() -> ?FOO(1, <caret>).", 1, "A, [B]");
  }

  public void testMacroOverloaded() {
    doMacroTest("-define(F(X, Y), {X, Y}).\n-define(F(X), X).\nbar() -> ?F(1, <caret>).", 1, "X", "X, [Y]");
  }

  public void testMacroInsideFunctionCall() {
    doMacroTest(FOO + "-define(BAR(X), X).\nbar() -> foo(?BAR(<caret>), 2).", 0, "[X]");
  }

  public void testMacroFromInclude() {
    myFixture.addFileToProject("a.hrl", "-define(FOO(A, B), {A, B}).");
    doMacroTest("-include(\"a.hrl\").\nbar() -> ?FOO(<caret>).", 0, "[A], B");
  }

  public void testMacroAsGenericFunctionCall() {
    doMacroTest("-define(DEF(N, V), N() -> V).\n?DEF(foo, <caret>).", 1, "N, [V]");
  }

  public void testMacroInIfdefElseBranches() {
    doMacroTest("-ifdef(TEST).\n-define(L(X), {a, X}).\n-else.\n-define(L(X), {b, X}).\n-endif.\nbar() -> ?L(<caret>).", 0, "[X]");
  }

  public void testUndefinedMacroDefinitionIsNotShown() {
    doMacroTest("-define(F(X), X).\n-undef(F).\n-define(F(X, Y), {X, Y}).\nbar() -> ?F(1, <caret>).", 1, "X, [Y]");
  }

  public void testUndefInsideIfdefIsIgnored() {
    doMacroTest("-define(F(X), X).\n-ifdef(NEVER).\n-undef(F).\n-endif.\nbar() -> ?F(<caret>).", 0, "[X]");
  }

  public void testUndefAndRedefinitionInsideIfdef() {
    doMacroTest("-define(F(X), X).\n-ifdef(OVERRIDE).\n-undef(F).\n-define(F(X, Y), {X, Y}).\n-endif.\nbar() -> ?F(<caret>).", 0, "[X]", "[X], Y");
  }

  public void testUndefInsideIfIsIgnored() {
    doMacroTest("-define(F(X), X).\n-if(?OTP_RELEASE > 20).\n-undef(F).\n-endif.\nbar() -> ?F(<caret>).", 0, "[X]");
  }

  public void testUndefAfterConditionalIsApplied() {
    doMacroTest("-define(F(X), X).\n-ifdef(T).\n-endif.\n-undef(F).\n-define(F(X, Y), {X, Y}).\nbar() -> ?F(1, <caret>).", 1, "X, [Y]");
  }

  public void testUndefInSameConditionalAsDefinitionIsApplied() {
    doMacroTest("-ifndef(G).\n-define(G, 1).\n-define(F(X), X).\n-undef(F).\n-define(F(X, Y), {X, Y}).\n-endif.\nbar() -> ?F(1, <caret>).", 1, "X, [Y]");
  }

  public void testUndefInSameConditionalAsCallIsApplied() {
    doMacroTest("-define(F(X), X).\n-ifdef(TEST).\n-undef(F).\n-define(F(X, Y), {X, Y}).\nbar() -> ?F(1, <caret>).\n-endif.", 1, "X, [Y]");
  }

  public void testUndefInSameConditionalAsDefinitionAndCallIsApplied() {
    doMacroTest("-ifdef(TEST).\n-define(F(X), X).\n-undef(F).\n-define(F(X, Y), {X, Y}).\nbar() -> ?F(1, <caret>).\n-endif.", 1, "X, [Y]");
  }

  public void testFunctionFromOtherModule() {
    myFixture.addFileToProject("other.erl", "-module(other).\n-export([baz/2]).\nbaz(A, B) -> {A, B}.\n");
    doTest("bar() -> other:baz(1, <caret>).", 1);
    assertInstanceOf(getItemsToShow()[0], ErlangFunctionClause.class);
  }

  public void testFunctionCallInsideMacro() {
    doTest(FOO + "-define(BAR(X), X).\nbar() -> ?BAR(foo(1, <caret>)).", 1);
    assertInstanceOf(getItemsToShow()[0], ErlangFunctionClause.class);
  }

  private Object[] myItemsToShow;

  private Object[] getItemsToShow() {
    return myItemsToShow;
  }

  private void doMacroTest(String text, int highlightedParameterIndex, String... expectedPresentations) {
    doTest(text, highlightedParameterIndex);
    ErlangParameterInfoHandler parameterInfoHandler = new ErlangParameterInfoHandler();
    List<String> presentations = new ArrayList<>();
    for (Object item : getItemsToShow()) {
      assertInstanceOf(item, ErlangMacrosDefinition.class);
      MockParameterInfoUIContext<PsiElement> uiContext = new MockParameterInfoUIContext<>((PsiElement) item);
      uiContext.setCurrentParameterIndex(highlightedParameterIndex);
      parameterInfoHandler.updateUI(item, uiContext);
      String presentation = uiContext.getText();
      int start = uiContext.getHighlightStart();
      int end = uiContext.getHighlightEnd();
      presentations.add(uiContext.isTextDisabled() ? presentation :
                        presentation.substring(0, start) + "[" + presentation.substring(start, end) + "]" + presentation.substring(end));
    }
    assertEquals(Arrays.asList(expectedPresentations), presentations);
  }

  private void doTest(String text, int highlightedParameterIndex) {
    myFixture.configureByText("a.erl", text);
    ErlangParameterInfoHandler parameterInfoHandler = new ErlangParameterInfoHandler();
    CreateParameterInfoContext createContext = new MockCreateParameterInfoContext(myFixture.getEditor(), myFixture.getFile());
    // the platform calls findElementForParameterInfo in a background read action and showParameterInfo on EDT,
    // so the items to show, which need resolve, must be computed by the former
    ErlangArgumentList list = findElementForParameterInfoInBackground(parameterInfoHandler, createContext);

    if (highlightedParameterIndex >= 0) {
      assertNotNull(list);
      Object[] itemsToShow = createContext.getItemsToShow();
      assertNotNull(itemsToShow);
      assertTrue(itemsToShow.length > 0);
      parameterInfoHandler.showParameterInfo(list, createContext);
      assertSame(itemsToShow, createContext.getItemsToShow());
      myItemsToShow = itemsToShow;
    }
    MockUpdateParameterInfoContext updateContext = new MockUpdateParameterInfoContext(myFixture.getEditor(), myFixture.getFile());
    ErlangArgumentList element = parameterInfoHandler.findElementForUpdatingParameterInfo(updateContext);
    if (element == null) {
      assertEquals(-1, highlightedParameterIndex);
    }
    else {
      assertNotNull(element);
      parameterInfoHandler.updateParameterInfo(element, updateContext);
      assertEquals(highlightedParameterIndex, updateContext.getCurrentParameter());
    }
  }

  private static ErlangArgumentList findElementForParameterInfoInBackground(@NotNull ErlangParameterInfoHandler handler,
                                                                           @NotNull CreateParameterInfoContext context) {
    try {
      return ApplicationManager.getApplication()
        .executeOnPooledThread(() -> ReadAction.compute(() -> handler.findElementForParameterInfo(context)))
        .get(1, TimeUnit.MINUTES);
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  @SuppressWarnings("UnusedDeclaration")
  public static class MockCreateParameterInfoContext implements CreateParameterInfoContext {
    private Object[] myItems;
    private PsiElement myHighlightedElement;
    private final Editor myEditor;
    private final PsiFile myFile;

    public MockCreateParameterInfoContext(@NotNull Editor editor, @NotNull PsiFile file) {
      myEditor = editor;
      myFile = file;
    }

    @Override
    public Object[] getItemsToShow() {
      return myItems;
    }

    @Override
    public void setItemsToShow(Object[] items) {
      myItems = items;
    }

    @Override
    public void showHint(PsiElement element, int offset, ParameterInfoHandler handler) {}

    @Override
    public int getParameterListStart() {
      return myEditor.getCaretModel().getOffset();
    }

    @Override
    public PsiElement getHighlightedElement() {
      return myHighlightedElement;
    }

    @Override
    public void setHighlightedElement(PsiElement elements) {
      myHighlightedElement = elements;
    }

    @Override
    public Project getProject() {
      return myFile.getProject();
    }

    @Override
    public PsiFile getFile() {
      return myFile;
    }

    @Override
    public int getOffset() {
      return myEditor.getCaretModel().getOffset();
    }

    @Override
    @NotNull
    public Editor getEditor() {
      return myEditor;
    }
  }
}
