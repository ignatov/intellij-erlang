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

import com.intellij.execution.console.ConsoleHistoryController;
import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangVarProcessor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.Map;

public final class ErlangConsoleView extends LanguageConsoleImpl {
  @Nullable private ConsoleHistoryController myHistoryController;
  @Nullable private OutputStreamWriter myProcessInputWriter;

  public ErlangConsoleView(@NotNull Project project) {
    super(project, "Erlang Console", ErlangLanguage.INSTANCE);

    setPrompt(">");
    PsiFile originalFile = getFile().getOriginalFile();
    originalFile.putUserData(ErlangPsiImplUtil.ERLANG_CONSOLE, this);
    originalFile.putUserData(ErlangVarProcessor.ERLANG_VARIABLE_CONTEXT, new HashMap<String, ErlangQVar>());
  }

  @Override
  protected void doAddPromptToHistory() {
  }

  @Override
  public void attachToProcess(@NotNull ProcessHandler processHandler) {
    super.attachToProcess(processHandler);
    OutputStream processInput = processHandler.getProcessInput();
    assert processInput != null;
    //noinspection IOResourceOpenedButNotSafelyClosed
    myProcessInputWriter = new OutputStreamWriter(processInput);
    myHistoryController = new ConsoleHistoryController("Erlang", null, this);
    myHistoryController.install();
    ErlangConsoleViewDirectory.getInstance().addConsole(this);
  }

  @Override
  public void dispose() {
    super.dispose();
    ErlangConsoleViewDirectory.getInstance().delConsole(this);
  }

  public void append(@NotNull final String text) {
    WriteCommandAction.runWriteCommandAction(getProject(), new Runnable() {
      public void run() {
        Document document = getCurrentEditor().getDocument();
        document.insertString(document.getTextLength(), text);
      }
    });
  }

  public void execute() {
    if (myProcessInputWriter == null || myHistoryController == null) {
      return;
    }

    EditorEx consoleEditor = getConsoleEditor();
    Document editorDocument = consoleEditor.getDocument();
    String text = editorDocument.getText();

    final Map<String, ErlangQVar> context = getFile().getOriginalFile().getUserData(ErlangVarProcessor.ERLANG_VARIABLE_CONTEXT);
    if (context != null) { // todo: process only successful statements
      getFile().accept(new ErlangRecursiveVisitor() {
        @Override
        public void visitQVar(@NotNull ErlangQVar o) {
          String name = o.getName();
          if (!context.containsKey(name)) context.put(name, o);
        }

        @Override
        public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
          String name = o.getNameIdentifier().getText();
          int size = o.getArgumentList().getExpressionList().size();
          if (name.equals("f") && size == 0) context.clear();
        }
      });
    }

    addToHistoryInner(new TextRange(0, text.length()), consoleEditor, true, true);
    myHistoryController.addToHistory(text);
    for (String line : text.split("\n")) {
      try {
        myProcessInputWriter.write(line + "\n");
        myProcessInputWriter.flush();
      } catch (IOException e) { // Ignore
      }
    }
  }
}
