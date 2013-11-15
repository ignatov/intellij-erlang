/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.editor.ex.EditorEx;
import org.jetbrains.annotations.NotNull;

public class ErlangConsoleExecuteAction extends AnAction {
  @Override
  public void update(@NotNull AnActionEvent actionEvent) {
    Presentation presentation = actionEvent.getPresentation();
    Editor editor = actionEvent.getData(CommonDataKeys.EDITOR);
    if (!(editor instanceof EditorEx) || ((EditorEx) editor).isRendererMode()) {
      presentation.setEnabled(false);
      return;
    }
    ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(editor);
    if (consoleView == null) {
      presentation.setEnabled(false);
      return;
    }
    presentation.setEnabled(consoleView.isRunning());
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent actionEvent) {
    Editor editor = actionEvent.getData(CommonDataKeys.EDITOR);
    if (editor != null) {
      ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(editor);
      if (consoleView != null) {
        consoleView.execute();
      }
    }
  }
}