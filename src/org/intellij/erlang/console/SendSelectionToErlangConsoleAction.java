/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.console;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

public class SendSelectionToErlangConsoleAction extends AnAction {
  @Override
  public void update(@NotNull AnActionEvent actionEvent) {
    Presentation presentation = actionEvent.getPresentation();
    presentation.setIcon(ErlangIcons.ERLANG_CONSOLE);
    presentation.setVisible(true);
    DataContext dataContext = actionEvent.getDataContext();
    PsiFile psiFile = CommonDataKeys.PSI_FILE.getData(dataContext);
    if (!(psiFile instanceof ErlangFile)) {
      presentation.setEnabled(false);
      return;
    }
    Editor editor = CommonDataKeys.EDITOR.getData(dataContext);
    if (editor == null || editor.getSelectionModel().getSelectedText() == null) {
      presentation.setEnabled(false);
      return;
    }
    Project project = editor.getProject();
    if (project == null) {
      presentation.setEnabled(false);
      return;
    }
    ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(project);
    presentation.setEnabled(consoleView != null && consoleView.isRunning());
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent actionEvent) {
    DataContext dataContext = actionEvent.getDataContext();
    Editor editor = CommonDataKeys.EDITOR.getData(dataContext);
    if (editor == null) {
      return;
    }
    Project project = editor.getProject();
    if (project == null) {
      return;
    }
    String selectedText = editor.getSelectionModel().getSelectedText();
    if (selectedText == null) {
      return;
    }
    ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(project);
    if (consoleView != null) {
      consoleView.append(selectedText);
      consoleView.execute();
    }
  }
}
