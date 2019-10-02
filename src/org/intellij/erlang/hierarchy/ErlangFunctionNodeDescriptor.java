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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ui.util.CompositeAppearance;
import com.intellij.psi.PsiFile;
import com.intellij.ui.SimpleTextAttributes;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

class ErlangFunctionNodeDescriptor extends HierarchyNodeDescriptor {
  public ErlangFunctionNodeDescriptor(Project project, ErlangFunction function) {
    super(project, null, function, false);
    CompositeAppearance.DequeEnd beginning = myHighlightedText.getBeginning();
    beginning.addText(ErlangPsiImplUtil.createFunctionPresentation(function));
    PsiFile file = function.getContainingFile();
    ErlangModule module = file instanceof ErlangFile ? ((ErlangFile) file).getModule() : null;
    if (module != null) {
      beginning.addText(module.getName() + ":", SimpleTextAttributes.GRAY_ATTRIBUTES);
    }
  }
}
