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

package org.intellij.erlang.dialyzer;

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
  name = "DialyzerSettings",
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/dialyzer.xml", scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public final class DialyzerSettings implements PersistentStateComponent<DialyzerSettings> {
  @NotNull
  private String myCurrentPltPath = "";

  @NotNull
  public static DialyzerSettings getInstance(@NotNull Project project) {
    DialyzerSettings persisted = ServiceManager.getService(project, DialyzerSettings.class);
    return persisted != null ? persisted : new DialyzerSettings();
  }

  @Nullable
  @Override
  public DialyzerSettings getState() {
    return this;
  }

  @Override
  public void loadState(@NotNull DialyzerSettings dialyzerSettings) {
    XmlSerializerUtil.copyBean(dialyzerSettings, this);
  }

  @NotNull
  public String getCurrentPltPath() {
    return myCurrentPltPath;
  }

  public void setCurrentPltPath(@NotNull String currentPltPath) {
    myCurrentPltPath = currentPltPath;
  }

  @Override
  public String toString() {
    return "DialyzerSettings(myCurrentPltPath='" + myCurrentPltPath + "')";
  }
}