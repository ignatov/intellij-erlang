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

import com.intellij.execution.console.ConsoleHistoryController;
import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.execution.console.LanguageConsoleViewImpl;
import com.intellij.execution.process.ConsoleHistoryModel;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import org.intellij.erlang.ErlangLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Comparator;

final class ErlangConsoleView extends LanguageConsoleViewImpl {
  private static final Comparator<AnAction> ourActionComparator = new Comparator<AnAction>() {
    @Override
    public int compare(@NotNull AnAction o1, @NotNull AnAction o2) {
      if (o1 instanceof ErlangConsoleExecuteAction) {
        if (o2 instanceof ErlangConsoleExecuteAction) {
          return 0;
        }
        else {
          return -1;
        }
      }
      else {
        if (o2 instanceof ErlangConsoleExecuteAction) {
          return 1;
        }
        else {
          return 0;
        }
      }
    }
  };

  @Nullable private ConsoleHistoryModel myConsoleHistoryModel;
  @Nullable private OutputStreamWriter myProcessInputWriter;

  public ErlangConsoleView(@NotNull Project project) {
    super(new LanguageConsoleImpl(project, "Erlang Console", ErlangLanguage.INSTANCE));
  }

  @Override
  public void attachToProcess(@NotNull ProcessHandler processHandler) {
    super.attachToProcess(processHandler);
    //noinspection IOResourceOpenedButNotSafelyClosed
    myProcessInputWriter = new OutputStreamWriter(processHandler.getProcessInput());
    myConsoleHistoryModel = new ConsoleHistoryModel();
    new ConsoleHistoryController("Erlang", null, getConsole(), myConsoleHistoryModel).install();
    ErlangConsoleViewDirectory.getInstance().addConsole(this);
  }

  @Override
  public void dispose() {
    super.dispose();
    ErlangConsoleViewDirectory.getInstance().delConsole(this);
  }

  @Override
  public Object getData(@NotNull String dataId) {
    // This is needed to make sure that ErlangConsoleExecuteAction has a
    // chance to process Ctrl+ENTER keystroke first.
    if (PlatformDataKeys.ACTIONS_SORTER.is(dataId)) {
      return ourActionComparator;
    }
    else {
      return super.getData(dataId);
    }
  }

  public void append(@NotNull final String text) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      public void run() {
        final Document document = getConsole().getCurrentEditor().getDocument();
        document.insertString(document.getTextLength(), text);
      }
    });
  }

  public void execute() {
    if (myProcessInputWriter == null || myConsoleHistoryModel == null) {
      return;
    }
    final EditorEx consoleEditor = getConsole().getConsoleEditor();
    final Document editorDocument = consoleEditor.getDocument();
    final String text = editorDocument.getText();
    getConsole().addCurrentToHistory(new TextRange(0, text.length()), true, true);
    myConsoleHistoryModel.addToHistory(text);
    for (String line : text.split("\n")) {
      try {
        myProcessInputWriter.write(line + "\n");
        myProcessInputWriter.flush();
      } catch (IOException e) { // Ignore
      }
    }
  }
}
