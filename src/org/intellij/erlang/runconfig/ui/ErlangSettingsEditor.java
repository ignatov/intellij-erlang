/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A settings editor that knows its project, for the custom-created components of its form.
 * The forms instrumentation calls {@code createUIComponents()} right after the superclass constructor,
 * before the constructor body of the form, so the project has to be stored by a superclass.
 */
public abstract class ErlangSettingsEditor<Settings> extends SettingsEditor<Settings> {
  private final @Nullable Project myProject;

  protected ErlangSettingsEditor(@Nullable Project project) {
    myProject = project;
  }

  protected @NotNull EnvironmentVariablesComponent createEnvironmentVariablesComponent() {
    return new EnvironmentVariablesComponent(myProject);
  }
}
