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

package org.intellij.erlang.settings;

import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ArrayUtil;
import org.apache.commons.lang.StringUtils;
import org.intellij.erlang.dialyzer.DialyzerSettings;
import org.intellij.erlang.emacs.EmacsSettings;
import org.intellij.erlang.rebar.settings.RebarConfigurationForm;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.utils.ExtProcessUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

/**
 * @author Maxim Vladimirsky, ignatov
 */
public class ErlangExternalToolsConfigurable implements SearchableConfigurable, Configurable.NoScroll {
  public static String ERLANG_RELATED_TOOLS = "Erlang External Tools";
  private JPanel myPanel;
  private TextFieldWithBrowseButton myEmacsPathSelector;
  private JTextField myEmacsVersionText;
  private RebarConfigurationForm myRebarConfigurationForm;
  private TextFieldWithBrowseButton myPltPathSelector;
  private String myPrevEmacsPath;
  private EmacsSettings myEmacsSettings;
  private RebarSettings myRebarSettings;
  private DialyzerSettings myDialyzerSettings;

  public ErlangExternalToolsConfigurable(@NotNull Project project) {
    myRebarSettings = RebarSettings.getInstance(project);
    myEmacsSettings = EmacsSettings.getInstance(project);
    myDialyzerSettings = DialyzerSettings.getInstance(project);
    myEmacsPathSelector.addBrowseFolderListener("Select Emacs executable", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myPltPathSelector.addBrowseFolderListener("Select dialyzer PLT", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
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

    if (StringUtils.isEmpty(myEmacsSettings.getEmacsPath()) && SystemInfo.isLinux) {
      String suggestedPath = "/usr/bin/emacs";
      File file = new File(suggestedPath);
      if (file.exists() && FileUtil.canExecute(file)) {
        myEmacsSettings.setEmacsPath(suggestedPath);
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
    myRebarConfigurationForm.createComponent();
    return myPanel;
  }

  @Override
  public boolean isModified() {
    String emacsSelectedPath = myEmacsPathSelector.getText();
    if (!myPrevEmacsPath.equals(emacsSelectedPath)) validateEmacsPath();

    return
         !myRebarSettings.getRebarPath().equals(myRebarConfigurationForm.getPath())
      || !myEmacsSettings.getEmacsPath().equals(emacsSelectedPath)
      || !myDialyzerSettings.getCurrentPltPath().equals(myPltPathSelector.getText());
  }

  @Override
  public void apply() throws ConfigurationException {
    myRebarSettings.setRebarPath(myRebarConfigurationForm.getPath());
    myEmacsSettings.setEmacsPath(myEmacsPathSelector.getText());
    myDialyzerSettings.setCurrentPltPath(myPltPathSelector.getText());
  }

  @Override
  public void reset() {
    myRebarConfigurationForm.setPath(myRebarSettings.getRebarPath());
    myEmacsPathSelector.setText(myEmacsSettings.getEmacsPath());
    myPltPathSelector.setText(myDialyzerSettings.getCurrentPltPath());
    validateEmacsPath();
  }

  @Override
  public void disposeUIResources() {
  }

  private void validateEmacsPath() {
    String version = ExtProcessUtil.restrictedTimeExec(myEmacsPathSelector.getText() + " --version", 3000);
    String[] split = StringUtils.split(version, "\n");
    if (StringUtils.containsIgnoreCase(version, "emacs") && split.length > 0) {
      myEmacsVersionText.setText(ArrayUtil.getFirstElement(split));
    }
    else {
      myEmacsVersionText.setText("N/A");
    }
  }
}
