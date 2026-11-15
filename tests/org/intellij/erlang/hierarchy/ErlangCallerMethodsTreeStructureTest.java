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

package org.intellij.erlang.hierarchy;

import com.intellij.ide.hierarchy.HierarchyBrowserBaseEx;
import com.intellij.ide.hierarchy.HierarchyNodeDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangCallerMethodsTreeStructureTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testSeeInFunctionBodyIsNotACall() {
    myFixture.configureByText("test.erl", """
      -module(test).
      foo() ->
        %% @see bar/1
        ok.
      baz() -> bar(1).
      bar(_) -> ok.
      """);
    ErlangFile file = (ErlangFile) myFixture.getFile();
    ErlangFunction bar = file.getFunction("bar", 1);
    assertNotNull(bar);
    ErlangCallerMethodsTreeStructure structure =
      new ErlangCallerMethodsTreeStructure(getProject(), bar, HierarchyBrowserBaseEx.SCOPE_ALL);
    Object[] callers = structure.buildChildren(structure.getBaseDescriptor());
    assertEquals(1, callers.length);
    assertEquals(file.getFunction("baz", 0), ((HierarchyNodeDescriptor) callers[0]).getPsiElement());
  }
}
