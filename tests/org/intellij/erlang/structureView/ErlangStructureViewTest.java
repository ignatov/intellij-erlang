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

package org.intellij.erlang.structureView;

import com.intellij.ide.structureView.newStructureView.StructureViewComponent;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.PlatformTestUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import javax.swing.tree.TreePath;

public class ErlangStructureViewTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testAutoscrollFromSourceSingleClauseFunction() {
    myFixture.configureByText(ErlangFileType.MODULE, """
      -module(m).
      foo() -> ok.
      bar(X) -> <caret>X + 1.
      """);
    doTest(findFunction("bar"));
  }

  public void testAutoscrollFromSourceMultiClauseFunction() {
    myFixture.configureByText(ErlangFileType.MODULE, """
      -module(m).
      foo() -> ok.
      bar(0) -> zero;
      bar(X) -> <caret>X + 1.
      """);
    doTest(findFunction("bar").getFunctionClauseList().get(1));
  }

  private ErlangFunction findFunction(String name) {
    for (ErlangFunction function : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangFunction.class)) {
      if (name.equals(function.getName())) return function;
    }
    throw new AssertionError("No function " + name);
  }

  private void doTest(PsiElement expected) {
    myFixture.testStructureView(component -> {
      Object current = component.getTreeModel().getCurrentEditorElement();
      assertEquals(expected, current);
      PlatformTestUtil.expandAll(component.getTree());
      TreePath path = PlatformTestUtil.waitForPromise(component.select(current, false));
      assertNotNull(path);
      assertEquals(expected, StructureViewComponent.unwrapValue(path.getLastPathComponent()));
    });
  }
}
