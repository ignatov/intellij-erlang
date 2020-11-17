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

package org.intellij.erlang.rebar.settings;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.DocumentAdapter;
import com.intellij.ui.components.labels.ActionLink;
import com.intellij.util.download.DownloadableFileDescription;
import com.intellij.util.download.DownloadableFileService;
import com.intellij.util.download.FileDownloader;
import org.intellij.erlang.rebar.runner.RebarRunningStateUtil;
import org.intellij.erlang.utils.ExtProcessUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.awt.*;
import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public class RebarConfigurationForm {
  private JPanel myPanel;
  private TextFieldWithBrowseButton myRebarPathSelector;
  private JTextField myRebarVersionText;
  private JPanel myLinkContainer;

  public RebarConfigurationForm() {
    myRebarPathSelector.addBrowseFolderListener("Select Rebar Executable", "", null,
                                                FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myRebarPathSelector.getTextField().getDocument().addDocumentListener(new DocumentAdapter() {
      @Override
      protected void textChanged(@NotNull DocumentEvent documentEvent) {
        validateRebarPath(RebarConfigurationForm.this.myRebarPathSelector.getText(), s -> myRebarVersionText.setText(s));
      }
    });
  }

  public void setPath(@NotNull String rebarPath) {
    if (!myRebarPathSelector.getText().equals(rebarPath)) {
      myRebarPathSelector.setText(rebarPath);
      validateRebarPath(myRebarPathSelector.getText(), s -> myRebarVersionText.setText(s));
    }
  }

  @NotNull
  public String getPath() {
    return myRebarPathSelector.getText();
  }

  public boolean isPathValid() {
    return myRebarVersionText.getText().startsWith("rebar");
  }

  private static void validateRebarPath(String rebarPath, Consumer<String> consumer) {
    File rebarFile = new File(rebarPath);
    if (!rebarFile.exists()) {
      consumer.accept("");
      return;
    }

    ApplicationManager.getApplication().executeOnPooledThread(
      () -> {
        ExtProcessUtil.ExtProcessOutput rebar = ExtProcessUtil.execAndGetFirstLine(3000, rebarPath, "--version");
        String version = rebar.getStdOut();

        if (version.startsWith("rebar")) {
          updateUI(consumer, version);
          return;
        }

        String escriptPath = RebarRunningStateUtil.findEscriptExecutable();
        ExtProcessUtil.ExtProcessOutput escript = ExtProcessUtil.execAndGetFirstLine(3000, escriptPath, rebarPath, "--version");
        String versionWithEscript = escript.getStdOut();

        if (versionWithEscript.startsWith("rebar")) {
          updateUI(consumer, versionWithEscript);
          return;
        }

        String rebarErr = rebar.getStdErr();
        if (StringUtil.isNotEmpty(rebarErr)) {
          updateUI(consumer, "Error: " + rebarErr);
          return;
        }

        String escriptErr = escript.getStdErr();
        if (StringUtil.isNotEmpty(escriptErr)) {
          updateUI(consumer, "Escript Error: " + escriptErr);
        }
      });
  }

  private static void updateUI(Consumer<String> consumer, String errMessage) {
    ApplicationManager.getApplication().invokeLater(() -> consumer.accept(errMessage), ModalityState.any());
  }

  private void createUIComponents() {
    myLinkContainer = new JPanel(new BorderLayout());
    myLinkContainer.add(createLink("Download the latest Rebar 3", "https://s3.amazonaws.com/rebar3/rebar3", "rebar3"), BorderLayout.NORTH);
    myLinkContainer.add(createLink("Download the latest Rebar", "https://github.com/rebar/rebar/wiki/rebar", "rebar"), BorderLayout.CENTER);
  }

  @NotNull
  private ActionLink createLink(@NotNull String title, final @NotNull String url, final @NotNull String fileName) {
    return new ActionLink(title, new AnAction() {
      @Override
      public void actionPerformed(@NotNull AnActionEvent e) {
        DownloadableFileService service = DownloadableFileService.getInstance();
        DownloadableFileDescription rebar = service.createFileDescription(url, fileName);
        FileDownloader downloader = service.createDownloader(Collections.singletonList(rebar), fileName);
        List<Pair<VirtualFile, DownloadableFileDescription>> pairs = downloader.downloadWithProgress(null, getEventProject(e), myLinkContainer);
        if (pairs != null) {
          for (Pair<VirtualFile, DownloadableFileDescription> pair : pairs) {
            try {
              String path = pair.first.getCanonicalPath();
              if (path != null) {
                FileUtil.setExecutable(new File(path));
                myRebarPathSelector.setText(path);
                validateRebarPath(RebarConfigurationForm.this.myRebarPathSelector.getText(), s -> myRebarVersionText.setText(s));
              }
            }
            catch (Exception ignore) {
            }
          }
        }
      }
    });
  }
}
