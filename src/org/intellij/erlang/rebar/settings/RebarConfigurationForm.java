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

package org.intellij.erlang.rebar.settings;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.DocumentAdapter;
import com.intellij.ui.components.labels.ActionLink;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.download.DownloadableFileDescription;
import com.intellij.util.download.DownloadableFileService;
import com.intellij.util.download.FileDownloader;
import org.intellij.erlang.utils.ExtProcessUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.awt.*;
import java.io.File;
import java.util.List;

/**
 * @author Maxim Vladimirsky, ignatov
 */
public class RebarConfigurationForm {
  private JPanel myPanel;
  private TextFieldWithBrowseButton myRebarPathSelector;
  private JTextField myRebarVersionText;
  private JPanel myLinkContainer;

  private boolean myRebarPathValid;

  public RebarConfigurationForm() {
    myRebarPathSelector.addBrowseFolderListener("Select Rebar executable", "", null,
      FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myRebarPathSelector.getTextField().getDocument().addDocumentListener(new DocumentAdapter() {
      @Override
      protected void textChanged(DocumentEvent documentEvent) {
        myRebarPathValid = validateRebarPath();
      }
    });
    myRebarPathValid = false;
  }

  public void setPath(@NotNull String rebarPath) {
    if (!myRebarPathSelector.getText().equals(rebarPath)) {
      myRebarPathSelector.setText(rebarPath);
      myRebarPathValid = validateRebarPath();
    }
  }

  @NotNull
  public String getPath() {
    return myRebarPathSelector.getText();
  }

  public boolean isPathValid() {
    return myRebarPathValid;
  }

  @Nullable
  public JComponent createComponent() {
    return myPanel;
  }

  private boolean validateRebarPath() {
    final String rebarPath = myRebarPathSelector.getText();
    if (new File(rebarPath).exists()) {
      String version = ExtProcessUtil.restrictedTimeExec(myRebarPathSelector.getText() + " --version", 3000);
      if (version.startsWith("rebar")) {
        myRebarVersionText.setText(version);
        return true;
      }
    }
    myRebarVersionText.setText("N/A");
    return false;
  }

  private void createUIComponents() {
    myLinkContainer = new JPanel(new BorderLayout());
    ActionLink link = new ActionLink("Download the latest Rebar version", new AnAction() {
      @Override
      public void actionPerformed(AnActionEvent e) {
        DownloadableFileService service = DownloadableFileService.getInstance();
        DownloadableFileDescription rebar = service.createFileDescription("https://github.com/rebar/rebar/wiki/rebar", "rebar");
        FileDownloader downloader = service.createDownloader(ContainerUtil.list(rebar), getEventProject(e), myLinkContainer, "rebar");
        List<Pair<VirtualFile, DownloadableFileDescription>> pairs = downloader.downloadAndReturnWithDescriptions();
        if (pairs != null) {
          for (Pair<VirtualFile, DownloadableFileDescription> pair : pairs) {
            try {
              String path = pair.first.getCanonicalPath();
              if (path != null) {
                FileUtilRt.setExecutableAttribute(path, true);
                myRebarPathSelector.setText(path);
                validateRebarPath();
              }
            } catch (Exception e1) { // Ignore
            }
          }
        }
      }
    });
    myLinkContainer.add(link, BorderLayout.NORTH);
  }
}
