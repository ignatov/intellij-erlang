/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.configuration;

import com.intellij.compiler.options.CompilerConfigurable;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ex.Settings;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.util.ObjectUtils;
import com.intellij.util.execution.ParametersListUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.settings.ErlangExternalToolsConfigurable;
import org.intellij.erlang.utils.AncestorAdapter;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.AncestorEvent;
import java.util.List;

public class ErlangCompilerOptionsConfigurable extends CompilerConfigurable {

  private JPanel myRootPanel;
  private JCheckBox myUseRebarCompilerCheckBox;
  private JButton myConfigureRebarButton;
  private JCheckBox myAddDebugInfoCheckBox;
  private RawCommandLineEditor myAdditionalErlcArgumentsEditor;
  private JLabel myAdditionalErlcArgumentsLabel;
  private final ErlangCompilerSettings mySettings;
  private final Project myProject;

  public ErlangCompilerOptionsConfigurable(Project project) {
    super(project);
    myProject = project;
    mySettings = ErlangCompilerSettings.getInstance(project);
    setupUiListeners();
  }

  private void setupUiListeners() {
    myConfigureRebarButton.addActionListener(e -> {
      DataContext context = DataManager.getInstance().getDataContext(myConfigureRebarButton);
      Settings settings = ObjectUtils.assertNotNull(Settings.KEY.getData(context));
      Configurable configurable = settings.find(ErlangExternalToolsConfigurable.ERLANG_RELATED_TOOLS);
      if (configurable != null) {
        settings.select(configurable);
      }
    });
    myRootPanel.addAncestorListener(new AncestorAdapter() {
      @Override
      public void ancestorAdded(AncestorEvent event) {
        reset();
      }
    });
    myUseRebarCompilerCheckBox.addItemListener(e -> setUseRebarCompiler(myUseRebarCompilerCheckBox.isSelected()));
  }

  @NotNull
  @Override
  public String getId() {
    return "Erlang compiler";
  }

  @Override
  public String getDisplayName() {
    return "Erlang Compiler";
  }

  @Override
  public JComponent createComponent() {
    return myRootPanel;
  }

  @Override
  public void reset() {
    boolean rebarPathIsSet = StringUtil.isNotEmpty(RebarSettings.getInstance(myProject).getRebarPath());
    myUseRebarCompilerCheckBox.setEnabled(rebarPathIsSet);
    myConfigureRebarButton.setVisible(!rebarPathIsSet);
    setUseRebarCompiler(rebarPathIsSet && mySettings.isUseRebarCompilerEnabled());
    myAddDebugInfoCheckBox.setSelected(mySettings.isAddDebugInfoEnabled());
    myAdditionalErlcArgumentsEditor.setText(argumentsString(mySettings.getAdditionalErlcArguments()));
  }

  @Override
  public void apply() {
    mySettings.setUseRebarCompilerEnabled(myUseRebarCompilerCheckBox.isSelected());
    mySettings.setAddDebugInfoEnabled(myAddDebugInfoCheckBox.isSelected());
    mySettings.setAdditionalErlcArguments(arguments(myAdditionalErlcArgumentsEditor.getText()));
  }

  @Override
  public boolean isModified() {
    return myUseRebarCompilerCheckBox.isSelected() != mySettings.isUseRebarCompilerEnabled() ||
           myAddDebugInfoCheckBox.isSelected() != mySettings.isAddDebugInfoEnabled() ||
           !StringUtil.equals(myAdditionalErlcArgumentsEditor.getText(),
                              argumentsString(mySettings.getAdditionalErlcArguments()));
  }

  private void setUseRebarCompiler(boolean useRebarCompiler) {
    myUseRebarCompilerCheckBox.setSelected(useRebarCompiler);

    myAdditionalErlcArgumentsLabel.setVisible(!useRebarCompiler);
    myAdditionalErlcArgumentsEditor.setVisible(!useRebarCompiler);
  }

  @NotNull
  private static String argumentsString(@NotNull List<String> arguments) {
    return ParametersListUtil.join(arguments);
  }

  @NotNull
  private static List<String> arguments(@NotNull String argumentsText) {
    return ParametersListUtil.parse(argumentsText);
  }
}
