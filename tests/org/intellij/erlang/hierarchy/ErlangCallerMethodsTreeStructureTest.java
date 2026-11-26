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

public class ErlangCallerMethodsTreeStructureTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testEveryCallerIsListedOnce() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("callers.erl", """
      -module(callers).
      -export([a/0, b/0]).
      a() -> c().
      b() -> c(), c().
      c() -> ok.
      d() -> ok.
      """);
    ErlangFunction c = file.getFunction("c", 0);
    assertNotNull(c);
    ErlangCallerMethodsTreeStructure structure =
      new ErlangCallerMethodsTreeStructure(getProject(), c, HierarchyBrowserBaseEx.SCOPE_ALL);
    List<String> callers = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      callers.add(((ErlangFunction)((HierarchyNodeDescriptor)child).getPsiElement()).getName());
    }
    assertSameElements(callers, "a", "b");
  }

  public void testThisClassScopeKeepsCallersOfTheSameFile() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("callers.erl", """
      -module(callers).
      -export([a/0, b/0]).
      a() -> c().
      b() -> c(), c().
      c() -> ok.
      d() -> ok.
      """);
    ErlangFunction c = file.getFunction("c", 0);
    assertNotNull(c);
    ErlangCallerMethodsTreeStructure structure =
      new ErlangCallerMethodsTreeStructure(getProject(), c, HierarchyBrowserBaseEx.SCOPE_CLASS);
    List<String> callers = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      callers.add(((ErlangFunction)((HierarchyNodeDescriptor)child).getPsiElement()).getName());
    }
    assertSameElements(callers, "a", "b");
  }
}
