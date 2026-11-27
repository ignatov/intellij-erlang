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
  public void testThisClassScopeKeepsCalleesOfTheSameFile() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("m.erl", """
      -module(m).
      -export([a/0]).
      a() -> c().
      c() -> ok.
      """);
    ErlangFunction a = file.getFunction("a", 0);
    assertNotNull(a);
    ErlangCalleeMethodsTreeStructure structure =
      new ErlangCalleeMethodsTreeStructure(getProject(), a, HierarchyBrowserBaseEx.SCOPE_CLASS);
    List<String> callees = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      callees.add(((ErlangFunction)((HierarchyNodeDescriptor)child).getPsiElement()).getName());
    }
    assertSameElements(callees, "c");
  }

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

  public void testThisClassScopeLeavesOutCalleesOfOtherFiles() {
    myFixture.addFileToProject("other.erl", """
      -module(other).
      -export([o/0]).
      o() -> ok.
      """);
    ErlangFile file = (ErlangFile)myFixture.configureByText("m.erl", """
      -module(m).
      -export([a/0]).
      a() -> other:o(), c().
      c() -> ok.
      """);
    ErlangFunction a = file.getFunction("a", 0);
    assertOrderedEquals(callees(a), "o/0", "c/0");
    assertOrderedEquals(callees(a, HierarchyBrowserBaseEx.SCOPE_CLASS), "c/0");
  }

  private List<String> callees(ErlangFunction function) {
    return callees(function, HierarchyBrowserBaseEx.SCOPE_ALL);
  }

  private List<String> callees(ErlangFunction function, String scopeType) {
    assertNotNull(function);
    ErlangCalleeMethodsTreeStructure structure =
      new ErlangCalleeMethodsTreeStructure(getProject(), function, scopeType);
    List<String> callees = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      ErlangFunction callee = (ErlangFunction)((HierarchyNodeDescriptor)child).getPsiElement();
      callees.add(callee.getName() + "/" + callee.getArity());
    }
    return callees;
  }
}
