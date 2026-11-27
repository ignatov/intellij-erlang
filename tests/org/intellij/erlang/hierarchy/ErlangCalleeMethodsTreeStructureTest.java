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

import java.util.ArrayList;
import java.util.List;

public class ErlangCalleeMethodsTreeStructureTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testEveryCalleeIsListedOnce() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("m.erl", """
      -module(m).
      -export([b/0]).
      b() -> c(), c().
      c() -> ok.
      """);
    assertOrderedEquals(callees(file.getFunction("b", 0)), "c/0");
  }

  public void testDifferentCalleesKeepCallOrder() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("m.erl", """
      -module(m).
      -export([b/0]).
      b() -> c(1), c(), c(2), b(), c().
      c() -> ok.
      c(_) -> ok.
      """);
    assertOrderedEquals(callees(file.getFunction("b", 0)), "c/1", "c/0", "b/0");
  }

  private List<String> callees(ErlangFunction function) {
    assertNotNull(function);
    ErlangCalleeMethodsTreeStructure structure =
      new ErlangCalleeMethodsTreeStructure(getProject(), function, HierarchyBrowserBaseEx.SCOPE_ALL);
    List<String> callees = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      ErlangFunction callee = (ErlangFunction)((HierarchyNodeDescriptor)child).getPsiElement();
      callees.add(callee.getName() + "/" + callee.getArity());
    }
    return callees;
  }
}
