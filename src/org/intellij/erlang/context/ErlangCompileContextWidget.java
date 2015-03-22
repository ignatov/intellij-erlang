/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.context;

import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.options.ShowSettingsUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.ListPopup;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.impl.status.EditorBasedWidget;
import com.intellij.util.Consumer;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.List;

public class ErlangCompileContextWidget extends EditorBasedWidget implements StatusBarWidget.MultipleTextValuesPresentation, StatusBarWidget.Multiframe {
  public static final String ID = ErlangCompileContext.class.getName();

  ErlangCompileContextWidget(@NotNull Project project) {
    super(project);
  }

  @NotNull
  @Override
  public String ID() {
    return ID;
  }

  @Nullable
  @Override
  public WidgetPresentation getPresentation(@NotNull PlatformType type) {
    return this;
  }


  @Nullable
  @Override
  public ListPopup getPopupStep() {
    ActionGroup actions = getActions();
    Component component = (Component) myStatusBar;
    DataContext dataContext = DataManager.getInstance().getDataContext(component);
    return JBPopupFactory.getInstance().createActionGroupPopup("Compile Contexts", actions, dataContext, JBPopupFactory.ActionSelectionAid.SPEEDSEARCH, false);
  }

  @Nullable
  @Override
  public String getSelectedValue() {
    Project project = getProject();
    return project != null ? "Context: " + ErlangCompileContextManager.getInstance(project).getActiveContextName() : null;
  }

  @NotNull
  @Override
  public String getMaxValue() {
    return "DUMMY";
  }

  @Nullable
  @Override
  public String getTooltipText() {
    return "Erlang Compile Context";
  }

  @Nullable
  @Override
  public Consumer<MouseEvent> getClickConsumer() {
    return null;
  }

  private ActionGroup getActions() {
    DefaultActionGroup group = new DefaultActionGroup();
    Project project = getProject();

    group.add(new EditContextsAction());

    group.addSeparator();

    List<String> availableContextNames = project == null ? ContainerUtil.<String>emptyList() : ErlangCompileContextManager.getInstance(project).getAvailableContextNames();
    for (String contextName : availableContextNames) {
      group.add(new ChangeContextAction(contextName));
    }

    return group;
  }

  @Override
  public StatusBarWidget copy() {
    return new ErlangCompileContextWidget(myProject);
  }

  private class ChangeContextAction extends AnAction {
    private final String myContextName;

    private ChangeContextAction(String contextName) {
      super(contextName);
      myContextName = contextName;
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
      Project project = getProject();
      if (project != null) {
        ErlangCompileContextManager.getInstance(project).setActiveContext(myContextName);
      }
    }
  }

  private class EditContextsAction extends AnAction {
    private EditContextsAction() {
      super("Edit Contexts...");
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
      ShowSettingsUtil.getInstance().showSettingsDialog(myProject, ErlangCompileContextListConfigurable.ID);
    }
  }
}
