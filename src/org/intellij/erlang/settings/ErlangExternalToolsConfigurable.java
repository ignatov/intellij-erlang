/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.settings;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.components.labels.ActionLink;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.download.DownloadableFileDescription;
import com.intellij.util.download.DownloadableFileService;
import com.intellij.util.download.FileDownloader;
import org.apache.commons.lang.StringUtils;
import org.intellij.erlang.emacs.EmacsSettings;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.concurrent.*;

/**
 * @author Maxim Vladimirsky, ignatov
 */
public class ErlangExternalToolsConfigurable implements SearchableConfigurable, Configurable.NoScroll {
  private static String ERLANG_RELATED_TOOLS = "Erlang External Tools";
  private JPanel myPanel;
  private TextFieldWithBrowseButton myRebarPathSelector;
  private JTextField myRebarVersionText;
  private RebarSettings myRebarSettings;
  private JPanel myLinkContainer;

  private String myPrevRebarPath;
  private TextFieldWithBrowseButton myEmacsPathSelector;
  private JTextField myEmacsVersionText;
  private String myPrevEmacsPath;
  private EmacsSettings myEmacsSettings;

  public ErlangExternalToolsConfigurable(@NotNull Project project) {
    myRebarSettings = RebarSettings.getInstance(project);
    myEmacsSettings = EmacsSettings.getInstance(project);
    myRebarPathSelector.addBrowseFolderListener("Select Rebar executable", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myEmacsPathSelector.addBrowseFolderListener("Select Emacs executable", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myPrevRebarPath = myRebarSettings.getRebarPath();
    myPrevEmacsPath = myEmacsSettings.getEmacsPath();

    if (StringUtils.isEmpty(myRebarSettings.getRebarPath())) {
      VirtualFile baseDir = project.getBaseDir();
      if (baseDir != null) {
        VirtualFile rebar = baseDir.findChild("rebar");
        if (rebar != null) {
          String canonicalPath = rebar.getCanonicalPath();
          if (canonicalPath != null) {
            myRebarSettings.setRebarPath(canonicalPath);
          }
        }
      }
    }

    reset();
  }

  @NotNull
  @Override
  public String getId() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public Runnable enableSearch(String option) {
    return null;
  }

  @NonNls
  @Override
  public String getDisplayName() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public String getHelpTopic() {
    return null;
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    return myPanel;
  }

  @Override
  public boolean isModified() {
    String rebarSelectedPath = myRebarPathSelector.getText();
    if (!myPrevRebarPath.equals(rebarSelectedPath)) validateRebarPath();

    String emacsSelectedPath = myEmacsPathSelector.getText();
    if (!myPrevEmacsPath.equals(emacsSelectedPath)) validateEmacsPath();

    return !myRebarSettings.getRebarPath().equals(rebarSelectedPath)
      || !myEmacsSettings.getEmacsPath().equals(emacsSelectedPath);
  }

  @Override
  public void apply() throws ConfigurationException {
    myRebarSettings.setRebarPath(myRebarPathSelector.getText());
    myEmacsSettings.setEmacsPath(myEmacsPathSelector.getText());
  }

  @Override
  public void reset() {
    myRebarPathSelector.setText(myRebarSettings.getRebarPath());
    validateRebarPath();
    myEmacsPathSelector.setText(myEmacsSettings.getEmacsPath());
    validateEmacsPath();
  }

  @Override
  public void disposeUIResources() {
  }

  private void validateRebarPath() {
    String version = restrictedTimeExec(myRebarPathSelector.getText() + " --version", 3000);
    if (version.startsWith("rebar")) {
      myRebarVersionText.setText(version);
    }
    else {
      myRebarVersionText.setText("N/A");
    }
  }

  private void validateEmacsPath() {
    String version = restrictedTimeExec(myEmacsPathSelector.getText() + " --version", 3000);
    String[] split = StringUtils.split(version, "\n");
    if (StringUtils.containsIgnoreCase(version, "emacs") && split.length > 0) {
      myEmacsVersionText.setText(ArrayUtil.getFirstElement(split));
    }
    else {
      myEmacsVersionText.setText("N/A");
    }
  }

  @NotNull
  private static String restrictedTimeExec(@NotNull String cmd, int timeout) {
    try {
      final Process cmdRunner = Runtime.getRuntime().exec(cmd);
      ExecutorService singleTreadExecutor = Executors.newSingleThreadExecutor();
      Future<String> cmdRunnerFuture = singleTreadExecutor.submit(new Callable<String>() {
        @Override
        public String call() throws Exception {
          cmdRunner.waitFor();
          BufferedReader outReader = new BufferedReader(new InputStreamReader(cmdRunner.getInputStream()));
          try {
            String firstLine = outReader.readLine();
            return firstLine == null ? "" : firstLine;
          } finally {
            outReader.close();
          }
        }
      });
      try {
        return cmdRunnerFuture.get(timeout, TimeUnit.MILLISECONDS);
      } catch (Exception e) { // Suppress
      }
      cmdRunnerFuture.cancel(true);
      singleTreadExecutor.shutdown();
    } catch (IOException e) { // Suppress
    }
    return "";
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
                if (StringUtils.isEmpty(myRebarSettings.getRebarPath()) || StringUtils.isEmpty(myRebarPathSelector.getText())) {
                  myRebarSettings.setRebarPath(path);
                  myRebarPathSelector.setText(myRebarSettings.getRebarPath());
                  validateRebarPath();
                }
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
