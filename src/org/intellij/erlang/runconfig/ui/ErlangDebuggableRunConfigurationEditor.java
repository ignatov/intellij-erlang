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

package org.intellij.erlang.runconfig.ui;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.util.Disposer;
import com.intellij.ui.HideableTitledPanel;
import org.intellij.erlang.runconfig.ErlangDebuggableRunConfigurationProducer;
import org.intellij.erlang.runconfig.ErlangRunConfigurationBase;
import org.jetbrains.annotations.NotNull;

public abstract class ErlangDebuggableRunConfigurationEditor<RunConfig extends ErlangRunConfigurationBase> extends SettingsEditor<RunConfig> {
  private final ErlangDebugOptionsEditorForm myDebugOptionsEditor = new ErlangDebugOptionsEditorForm();

  protected ErlangDebuggableRunConfigurationEditor() {
    Disposer.register(this, myDebugOptionsEditor);
  }

  @Override
  protected final void resetEditorFrom(@NotNull RunConfig runConfig) {
    ErlangDebuggableRunConfigurationProducer.updateDebugOptions(runConfig);
    myDebugOptionsEditor.resetFrom(runConfig.getDebugOptions());
    doResetEditorFrom(runConfig);
  }

  protected abstract void doResetEditorFrom(RunConfig runConfig);

  @Override
  protected final void applyEditorTo(RunConfig runConfig) throws ConfigurationException {
    myDebugOptionsEditor.applyTo(runConfig.getDebugOptions());
    doApplyEditorTo(runConfig);
  }

  protected abstract void doApplyEditorTo(RunConfig runConfig);


  protected HideableTitledPanel createDebugOptionsHideablePanel() {
    HideableTitledPanel hideablePanel = new HideableTitledPanel("&Debug Options", false);
    hideablePanel.setOn(false);
    hideablePanel.setContentComponent(myDebugOptionsEditor.getComponent());
    return hideablePanel;
  }
}
