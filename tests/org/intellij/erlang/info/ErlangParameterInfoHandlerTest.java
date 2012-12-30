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

package org.intellij.erlang.info;

import com.intellij.lang.parameterInfo.CreateParameterInfoContext;
import com.intellij.lang.parameterInfo.ParameterInfoHandler;
import com.intellij.lang.parameterInfo.UpdateParameterInfoContext;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangParameterInfoHandlerTest extends LightCodeInsightFixtureTestCase {
  private static final String FOO = "foo(Arg1, Arg2) -> ok.\n ";

  public void testEmpty() { doTest(FOO + "bar() -> foo(<caret>)", 0); }
  public void testSecond() { doTest(FOO + "bar() -> foo(1, <caret>)", 1); }

  private void doTest(String text, int highlightedParameterIndex) {
    myFixture.configureByText("a.erl", text);
    final ParameterInfoHandler paramInfoHandler = new ErlangParameterInfoHandler();
//    final CreateParameterInfoContext parameterInfoContext = new MockCreateParameterInfoContext(myFixture.getEditor(), myFixture.getFile());
//    final Object list = paramInfoHandler.findElementForParameterInfo(parameterInfoContext);

//    if (highlightedParameterIndex >= 0) {
//      assertNotNull(list);
//      Object[] itemsToShow = parameterInfoContext.getItemsToShow();
//      assertNotNull(itemsToShow);
//      assertTrue(itemsToShow.length > 0);
//    }
    MockUpdateParameterInfoContext updateContext = new MockUpdateParameterInfoContext(myFixture.getEditor(), myFixture.getFile());
    final Object element = paramInfoHandler.findElementForUpdatingParameterInfo(updateContext);
    if (element == null) {
      assertEquals(-1, highlightedParameterIndex);
    }
    else {
      assertNotNull(element);
      //noinspection unchecked
      paramInfoHandler.updateParameterInfo(element, updateContext);
      assertEquals(highlightedParameterIndex, updateContext.getCurrentParameter());
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

  @SuppressWarnings("UnusedDeclaration")
  public static class MockUpdateParameterInfoContext implements UpdateParameterInfoContext {
    private final Editor myEditor;
    private final PsiFile myFile;
    private PsiElement myParameterOwner;
    private Object myHighlightedParameter;
    private int myCurrentParameter;

    public MockUpdateParameterInfoContext(@NotNull Editor editor, @NotNull PsiFile file) {
      myEditor = editor;
      myFile = file;
    }

    public void removeHint() {}

    public void setParameterOwner(PsiElement o) {
      myParameterOwner = o;
    }

    public PsiElement getParameterOwner() { return myParameterOwner; }

    public void setHighlightedParameter(Object parameter) {
      myHighlightedParameter = parameter;
    }

    public void setCurrentParameter(int index) {
      myCurrentParameter = index;
    }

    public int getCurrentParameter() {
      return myCurrentParameter;
    }

    public boolean isUIComponentEnabled(int index) { return false; }

    public void setUIComponentEnabled(int index, boolean b) {}

    public int getParameterListStart() {
      return myEditor.getCaretModel().getOffset();
    }

    public Object[] getObjectsToView() {
      return ArrayUtil.EMPTY_OBJECT_ARRAY;
    }

    public Project getProject() {
      return myFile.getProject();
    }

    public PsiFile getFile() {
      return myFile;
    }

    public int getOffset() {
      return myEditor.getCaretModel().getOffset();
    }

    @NotNull
    public Editor getEditor() {
      return myEditor;
    }
  }


}
