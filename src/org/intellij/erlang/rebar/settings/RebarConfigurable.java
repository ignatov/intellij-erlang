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

package org.intellij.erlang.rebar.settings;

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.*;

final class RebarConfigurable implements SearchableConfigurable, Configurable.NoScroll  {
  private JPanel myPanel;
  private TextFieldWithBrowseButton myRebarPathSelector;
  private JTextField myRebarVersionText;

  private String myPrevRebarPath;
  private RebarSettings myRebarSettings;

  public RebarConfigurable(@NotNull Project project) {
    myRebarSettings = RebarSettings.getInstance(project);
    myRebarPathSelector.addBrowseFolderListener("Select rebar executable", "", null,
      FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myPrevRebarPath = myRebarSettings.getRebarPath();
    myRebarPathSelector.setText(myRebarSettings.getRebarPath());
    validateRebarPath();
  }

  @NotNull
  @Override
  public String getId() {
    return "Rebar";
  }

  @Nullable
  @Override
  public Runnable enableSearch(String option) {
    return null;
  }

  @NonNls
  @Override
  public String getDisplayName() {
    return "Rebar";
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
    final String selectedPath = myRebarPathSelector.getText();
    if (!myPrevRebarPath.equals(selectedPath)) {
      validateRebarPath();
    }
    return !myRebarSettings.getRebarPath().equals(selectedPath);
  }

  @Override
  public void apply() throws ConfigurationException {
    myRebarSettings.setRebarPath(myRebarPathSelector.getText());
  }

  @Override
  public void reset() {
    myRebarPathSelector.setText(myRebarSettings.getRebarPath());
    validateRebarPath();
  }

  @Override
  public void disposeUIResources() {
  }

  private void validateRebarPath() {
    final String version = restrictedTimeExec(myRebarPathSelector.getText() + " --version", 3000);
    if (version.startsWith("rebar")) {
      myRebarVersionText.setText(version);
    }
    else {
      myRebarVersionText.setText("N/A");
    }
  }

  @NotNull
  private static String restrictedTimeExec(@NotNull String cmd, int timeout) {
    try {
      final Process cmdRunner = Runtime.getRuntime().exec(cmd);
      final ExecutorService singleTreadExecutor = Executors.newSingleThreadExecutor();
      final Future<String> cmdRunnerFuture = singleTreadExecutor.submit(new Callable<String>() {
        @Override
        public String call() throws Exception {
          cmdRunner.waitFor();
          final BufferedReader outReader = new BufferedReader(new InputStreamReader(cmdRunner.getInputStream()));
          try {
            final String firstLine = outReader.readLine();
            return firstLine == null ? "" : firstLine;
          }
          finally {
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
}
