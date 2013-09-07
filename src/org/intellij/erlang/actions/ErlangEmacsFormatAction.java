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
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.ExceptionUtil;
import org.intellij.erlang.emacs.EmacsSettings;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.intellij.erlang.utils.ErlangExternalToolsNotificationListener;
import org.jetbrains.annotations.NotNull;

import java.io.File;

/**
 * @author ignatov
 */
public class ErlangEmacsFormatAction extends AnAction implements DumbAware {
  private static final String NOTIFICATION_TITLE = "Reformat code with Emacs";
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
    if (!(psiFile instanceof ErlangFile)) return;
    VirtualFile virtualFile = psiFile.getVirtualFile();
    if (virtualFile == null) return;

    final String groupId = e.getPresentation().getText();
    try {
      final File tmpFile = FileUtil.createTempFile("emacs", ".erl", true);

      final GeneralCommandLine commandLine = new GeneralCommandLine();
      String emacsPath = EmacsSettings.getInstance(project).getEmacsPath();
      if (emacsPath.isEmpty()) {
        Notifications.Bus.notify(
          new Notification(groupId, NOTIFICATION_TITLE,
            "Emacs executable path is empty"+
            "<br/><a href='configure'>Configure</a>",
          NotificationType.WARNING, new ErlangExternalToolsNotificationListener(project)), project);
        return;
      }
      commandLine.setExePath(emacsPath);
      commandLine.addParameters("--batch", "--eval");

      String homePath = getSdkPath(project);

      if (StringUtil.isEmpty(homePath)) {
        Notifications.Bus.notify(
          new Notification(groupId, NOTIFICATION_TITLE, "Erlang project SDK is not configured",
            NotificationType.WARNING), project);
        return;
      }
      String s = "\n" +
        "(progn (find-file \"" + virtualFile.getCanonicalPath() + "\")\n" +
        "    (setq erlang-root-dir \"" + homePath +"\")\n" +
        "    (setq load-path (cons (car (file-expand-wildcards (concat erlang-root-dir \"/lib/tools-*/emacs\")))\n" +
        "                          load-path))\n" +
        "    (require 'erlang-start)\n" +
        "    (erlang-mode)\n" +
        "    (erlang-indent-current-buffer)\n" +
        "    (delete-trailing-whitespace)\n" +
        "    (untabify (point-min) (point-max))\n" +
        "    (write-region (point-min) (point-max) \"" + tmpFile.getCanonicalPath() + "\")\n" +
        "    (kill-emacs))";

      commandLine.addParameter(s);

      ApplicationManager.getApplication().saveAll();

      final String commandLineString = commandLine.getCommandLineString();
      OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLineString);
      handler.addProcessListener(new ProcessAdapter() {
        @Override
        public void processTerminated(ProcessEvent event) {
          ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
              try {
                final String emacsText = FileUtilRt.loadFile(tmpFile, true);
                if (StringUtil.isEmptyOrSpaces(emacsText)) {
                  Notifications.Bus.notify(new Notification(groupId, NOTIFICATION_TITLE,
                    "Emacs returned an empty file",
                    NotificationType.WARNING), project);
                  LOG.warn("Emacs returned an empty file:\n" + commandLineString);
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
                }, NOTIFICATION_TITLE, "", document);

                Notifications.Bus.notify(new Notification(groupId, NOTIFICATION_TITLE,
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

  @NotNull
  private static String getSdkPath(@NotNull Project project) {
    if (ErlangSystemUtil.isSmallIde()) {
      return ErlangExternalToolsConfigurable.getErlangSdkPath(project);
    }
    else {
      Sdk projectSdk = ProjectRootManager.getInstance(project).getProjectSdk();
      return projectSdk != null ? StringUtil.notNullize(projectSdk.getHomePath()) : "";
    }
  }
}
