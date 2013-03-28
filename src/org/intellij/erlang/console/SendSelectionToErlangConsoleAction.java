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
    final Presentation presentation = actionEvent.getPresentation();
    presentation.setIcon(ErlangIcons.ERLANG_CONSOLE);
    presentation.setVisible(true);
    final DataContext dataContext = actionEvent.getDataContext();
    final PsiFile psiFile = LangDataKeys.PSI_FILE.getData(dataContext);
    if (!(psiFile instanceof ErlangFile)) {
      presentation.setEnabled(false);
      return;
    }
    final Editor editor = PlatformDataKeys.EDITOR.getData(dataContext);
    if (editor == null || editor.getSelectionModel().getSelectedText() == null) {
      presentation.setEnabled(false);
      return;
    }
    final Project project = editor.getProject();
    if (project == null) {
      presentation.setEnabled(false);
      return;
    }
    final ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(project);
    presentation.setEnabled(consoleView != null && consoleView.isRunning());
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent actionEvent) {
    final DataContext dataContext = actionEvent.getDataContext();
    final Editor editor = PlatformDataKeys.EDITOR.getData(dataContext);
    if (editor == null) {
      return;
    }
    final Project project = editor.getProject();
    if (project == null) {
      return;
    }
    final String selectedText = editor.getSelectionModel().getSelectedText();
    if (selectedText == null) {
      return;
    }
    final ErlangConsoleView consoleView = ErlangConsoleViewDirectory.getInstance().getConsole(project);
    if (consoleView != null) {
      consoleView.append(selectedText);
      consoleView.execute();
    }
  }
}
