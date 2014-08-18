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

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.jps.rebar.JpsRebarSettingsSerializer;
import org.intellij.erlang.jps.rebar.RebarSettingsState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
  name = JpsRebarSettingsSerializer.REBAR_COMPONENT_NAME,
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/" + JpsRebarSettingsSerializer.REBAR_CONFIG_FILE_NAME, scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public final class RebarSettings implements PersistentStateComponent<RebarSettingsState> {
  @NotNull private RebarSettingsState myRebarSettingsState = new RebarSettingsState();

  @NotNull
  public static RebarSettings getInstance(@NotNull Project project) {
    RebarSettings persisted = ServiceManager.getService(project, RebarSettings.class);
    return persisted != null ? persisted : new RebarSettings();
  }

  @Nullable
  @Override
  public RebarSettingsState getState() {
    return myRebarSettingsState;
  }

  @Override
  public void loadState(@NotNull RebarSettingsState rebarSettings) {
    myRebarSettingsState = rebarSettings;
  }

  @NotNull
  public String getRebarPath() {
    return myRebarSettingsState.myRebarPath;
  }

  public void setRebarPath(@NotNull String rebarPath) {
    myRebarSettingsState.myRebarPath = rebarPath;
  }

  @Override
  public String toString() {
    return "RebarSettings(state='" + myRebarSettingsState.toString() + "')";
  }
}
