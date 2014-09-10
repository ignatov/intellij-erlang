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

package org.intellij.erlang.configuration;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleConfigurationEditor;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.roots.ui.configuration.ClasspathEditor;
import com.intellij.openapi.roots.ui.configuration.ModuleConfigurationEditorProvider;
import com.intellij.openapi.roots.ui.configuration.ModuleConfigurationState;
import com.intellij.openapi.roots.ui.configuration.OutputEditor;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.roots.ErlangContentEntriesEditor;

import javax.swing.*;

public class DefaultModuleEditorsProvider implements ModuleConfigurationEditorProvider {
  public ModuleConfigurationEditor[] createEditors(ModuleConfigurationState state) {
    Module module = state.getRootModel().getModule();
    if (ModuleType.get(module) instanceof ErlangModuleType) {
      return new ModuleConfigurationEditor[]{
        new ErlangContentEntriesEditor(module.getName(), state),
        new OutputEditorEx(state),
        new ClasspathEditor(state)
      };
    }
    return ModuleConfigurationEditor.EMPTY;
  }

  static class OutputEditorEx extends OutputEditor {
    protected OutputEditorEx(ModuleConfigurationState state) {
      super(state);
    }

    protected JComponent createComponentImpl() {
      JComponent component = super.createComponentImpl();
      component.remove(1); // todo: looks ugly
      return component;
    }
  }
}
