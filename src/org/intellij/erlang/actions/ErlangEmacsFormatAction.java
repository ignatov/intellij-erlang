/*
 * Copyright 2013 Sergey Ignatov
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

package org.intellij.erlang.actions;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.ExceptionUtil;
import org.intellij.erlang.psi.ErlangFile;

import javax.swing.*;
import java.io.File;
import java.text.MessageFormat;

/**
 * @author ignatov
 */
public class ErlangEmacsFormatAction extends AnAction implements DumbAware {
  private static final Logger LOG = Logger.getInstance(ErlangEmacsFormatAction.class);

  @Override
  public void update(AnActionEvent e) {
    final PsiFile psiFile = e.getData(LangDataKeys.PSI_FILE);
    boolean isErlang = psiFile instanceof ErlangFile;
    e.getPresentation().setEnabled(isErlang);
  }

  @Override
  public void actionPerformed(final AnActionEvent e) {
    final PsiFile psiFile = e.getData(LangDataKeys.PSI_FILE);
    final Project project = getEventProject(e);
    if (project == null) return;
    if (psiFile == null) return;
    if (!(psiFile instanceof ErlangFile)) return;
    VirtualFile virtualFile = psiFile.getVirtualFile();
    if (virtualFile == null) return;

    final String groupId = e.getPresentation().getText();
    try {
      final File tmpFile = FileUtil.createTempFile("emacs", ".erl", true);

      final GeneralCommandLine commandLine = new GeneralCommandLine();
      commandLine.setExePath("emacs");
      commandLine.addParameters("--batch", "--eval");

      String s = "\n" +
        "(progn (find-file \"{0}\")\n" +
//        Mac OS specific settings
//        "    (if (string-equal \"darwin\" (symbol-name system-type))\n" +
//        "        (setq erlang-root-dir (car (file-expand-wildcards \"/usr/local/Cellar/erlang/R*\")))\n" +
//        "        (setq erlang-root-dir \"/usr/lib/erlang/\"))\n" +
//        "    (setq load-path (cons (car (file-expand-wildcards (concat erlang-root-dir \"/lib/erlang/lib/tools-*/emacs\"))) load-path))\n" +
        "    (require ''erlang-start)\n" +
        "    (erlang-mode)\n" +
        "    (untabify (point-min) (point-max))\n" +
        "    (delete-trailing-whitespace)\n" +
        "    (erlang-indent-current-buffer)\n" +
        "    (write-region (point-min) (point-max) \"{1}\")\n" +
        "    (kill-emacs))";

      commandLine.addParameter(MessageFormat.format(s, virtualFile.getCanonicalPath(), tmpFile.getCanonicalPath()));

      ApplicationManager.getApplication().saveAll();

      OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
      handler.addProcessListener(new ProcessAdapter() {
        @Override
        public void processTerminated(ProcessEvent event) {
          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              try {
                final String emacsText = FileUtilRt.loadFile(tmpFile);
                if (StringUtil.isEmptyOrSpaces(emacsText)) {
                  Notifications.Bus.notify(new Notification(groupId,
                    "Reformat code with Emacs",
                    "Emacs returned an empty file",
                    NotificationType.WARNING), project);
                  return;
                }
                final Document document = PsiDocumentManager.getInstance(project).getDocument(psiFile);
                if (document == null) return;
                CommandProcessor.getInstance().executeCommand(project, new Runnable() {
                  @Override
                  public void run() {
                    ApplicationManager.getApplication().runWriteAction(new Runnable() {
                      @Override
                      public void run() {
                        document.setText(emacsText);
                      }
                    });
                  }
                }, "Reformat code with Emacs", "", document);

                Notifications.Bus.notify(new Notification(groupId,
                  "Reformat code with Emacs",
                  psiFile.getName() + " formatted with Emacs",
                  NotificationType.INFORMATION), project);

              } catch (Exception ex) {
                Notifications.Bus.notify(new Notification(groupId,
                  psiFile.getName() + " formatting with Emacs failed", ExceptionUtil.getUserStackTrace(ex, LOG),
                  NotificationType.ERROR), project);
                LOG.error(ex);
              }
            }
          });
        }
      });
      handler.startNotify();
    } catch (Exception ex) {
      Notifications.Bus.notify(new Notification(groupId,
        psiFile.getName() + " formatting with Emacs failed", ExceptionUtil.getUserStackTrace(ex, LOG),
        NotificationType.ERROR), project);
      LOG.error(ex);
    }
  }
}
