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
import com.intellij.ide.util.treeView.AlphaComparator;
import com.intellij.ide.util.treeView.NodeDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangFunctionNodeDescriptorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testAlphaComparatorOrdersCalleesByFunctionName() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("m.erl", """
      -module(m).
      -export([f/0]).
      f() -> z(), a().
      z() -> ok.
      a() -> ok.
      """);
    ErlangFunction f = file.getFunction("f", 0);
    assertNotNull(f);
    ErlangCalleeMethodsTreeStructure structure =
      new ErlangCalleeMethodsTreeStructure(getProject(), f, HierarchyBrowserBaseEx.SCOPE_ALL);
    List<NodeDescriptor<?>> children = new ArrayList<>();
    for (Object child : structure.getChildElements(structure.getRootElement())) {
      HierarchyNodeDescriptor descriptor = (HierarchyNodeDescriptor)child;
      descriptor.update();
      children.add(descriptor);
    }
    children.sort(AlphaComparator.getInstance());
    List<String> names = new ArrayList<>();
    for (NodeDescriptor<?> descriptor : children) {
      names.add(((ErlangFunction)((HierarchyNodeDescriptor)descriptor).getPsiElement()).getName());
    }
    assertEquals(List.of("a", "z"), names);
  }
}
