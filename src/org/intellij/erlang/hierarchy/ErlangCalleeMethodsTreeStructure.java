/*
 * Copyright 2012-2019 Sergey Ignatov
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

import com.intellij.ide.hierarchy.HierarchyNodeDescriptor;
import com.intellij.ide.hierarchy.HierarchyTreeStructure;
import com.intellij.openapi.project.Project;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.jetbrains.annotations.NotNull;

public class ErlangCalleeMethodsTreeStructure extends HierarchyTreeStructure {
  private final String myCurrentScopeType;

  public ErlangCalleeMethodsTreeStructure(Project project, ErlangFunction psiElement, String currentScopeType) {
    super(project, new ErlangFunctionNodeDescriptor(project, psiElement));
    myCurrentScopeType = currentScopeType;
  }

  @NotNull
  @Override
  protected Object @NotNull [] buildChildren(@NotNull HierarchyNodeDescriptor descriptor) {
    ErlangFunction function = ObjectUtils.tryCast(descriptor.getPsiElement(), ErlangFunction.class);
    return SyntaxTraverser
      .psiTraverser()
      .withRoot(function)
      .filter(ErlangFunctionCallExpression.class)
      .map(o -> o.getReference().resolve())
      .filter(ErlangFunction.class)
      .filter(f -> isInScope(function, f, myCurrentScopeType))
      .map(f -> new ErlangFunctionNodeDescriptor(myProject, f))
      .toArray(new ErlangFunctionNodeDescriptor[0]);
  }
}
