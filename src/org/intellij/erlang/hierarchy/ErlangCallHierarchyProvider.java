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

import com.intellij.ide.hierarchy.*;
import com.intellij.ide.hierarchy.call.CallHierarchyBrowser;
import com.intellij.ide.util.treeView.AlphaComparator;
import com.intellij.ide.util.treeView.NodeDescriptor;
import com.intellij.ide.util.treeView.SourceComparator;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.PopupHandler;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Comparator;
import java.util.Map;

public class ErlangCallHierarchyProvider implements HierarchyProvider {
  private final static Logger LOG = Logger.getInstance(ErlangCallHierarchyProvider.class);

  @Nullable
  @Override
  public PsiElement getTarget(@NotNull DataContext dataContext) {
    return PsiTreeUtil.getParentOfType(dataContext.getData(CommonDataKeys.PSI_ELEMENT), ErlangFunction.class, false);
  }

  @NotNull
  @Override
  public HierarchyBrowser createHierarchyBrowser(@NotNull PsiElement target) {
    return new MyCallHierarchyBrowserBase(target.getProject(), target);
  }

  @Override
  public void browserActivated(@NotNull HierarchyBrowser hierarchyBrowser) {
    ((CallHierarchyBrowserBase) hierarchyBrowser).changeView(CallHierarchyBrowserBase.CALLER_TYPE);
  }

  private static class MyCallHierarchyBrowserBase extends CallHierarchyBrowserBase {
    public MyCallHierarchyBrowserBase(@NotNull Project project, @NotNull PsiElement function) {
      super(project, function);
    }

    @Nullable
    @Override
    protected PsiElement getElementFromDescriptor(@NotNull HierarchyNodeDescriptor descriptor) {
      return ObjectUtils.tryCast(descriptor.getPsiElement(), PsiElement.class);
    }

    @Override
    protected void createTrees(@NotNull Map<String, JTree> trees) {
      ActionGroup group = (ActionGroup) ActionManager.getInstance().getAction(IdeActions.GROUP_CALL_HIERARCHY_POPUP);
      final JTree tree1 = createTree(false);
      PopupHandler.installPopupHandler(tree1, group, ActionPlaces.CALL_HIERARCHY_VIEW_POPUP, ActionManager.getInstance());
      final CallHierarchyBrowser.BaseOnThisMethodAction baseOnThisMethodAction = new CallHierarchyBrowser.BaseOnThisMethodAction();
      baseOnThisMethodAction
        .registerCustomShortcutSet(ActionManager.getInstance().getAction(IdeActions.ACTION_CALL_HIERARCHY).getShortcutSet(), tree1);
      trees.put(CALLEE_TYPE, tree1);

      final JTree tree2 = createTree(false);
      PopupHandler.installPopupHandler(tree2, group, ActionPlaces.CALL_HIERARCHY_VIEW_POPUP, ActionManager.getInstance());
      baseOnThisMethodAction
        .registerCustomShortcutSet(ActionManager.getInstance().getAction(IdeActions.ACTION_CALL_HIERARCHY).getShortcutSet(), tree2);
      trees.put(CALLER_TYPE, tree2);
    }

    @Override
    protected boolean isApplicableElement(@NotNull PsiElement element) {
      return element instanceof ErlangFunction;
    }

    @Nullable
    @Override
    protected HierarchyTreeStructure createHierarchyTreeStructure(@NotNull String type, @NotNull PsiElement e) {
      if (CALLER_TYPE.equals(type)) return new ErlangCallerMethodsTreeStructure(myProject, (ErlangFunction)e, getCurrentScopeType());
      if (CALLEE_TYPE.equals(type)) return new ErlangCalleeMethodsTreeStructure(myProject, (ErlangFunction)e, getCurrentScopeType());
      LOG.error("unexpected type: " + type);
      return null;
    }

    @Nullable
    @Override
    protected Comparator<NodeDescriptor<?>> getComparator() {
      HierarchyBrowserManager.State state = HierarchyBrowserManager.getInstance(myProject).getState();
      return state != null && state.SORT_ALPHABETICALLY ? AlphaComparator.INSTANCE : SourceComparator.INSTANCE;
    }
  }
}
