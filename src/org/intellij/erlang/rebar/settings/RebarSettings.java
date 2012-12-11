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

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

@State(
  name = "RebarSettings",
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/rebar.xml", scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public final class RebarSettings implements PersistentStateComponent<RebarSettings> {
  @NotNull private String myRebarPath = "";

  @NotNull
  public static RebarSettings getInstance(@NotNull Project project) {
    final RebarSettings persisted = ServiceManager.getService(project, RebarSettings.class);
    return persisted != null ? persisted : new RebarSettings();
  }

  @Nullable
  @Override
  public RebarSettings getState() {
    return this;
  }

  @Override
  public void loadState(@NotNull RebarSettings rebarSettings) {
    XmlSerializerUtil.copyBean(rebarSettings, this);
  }

  @NotNull
  public String getRebarPath() {
    return myRebarPath;
  }

  public void setRebarPath(@NotNull String rebarPath) {
    myRebarPath = rebarPath;
  }

  @Override
  public String toString() {
    return "RebarSettings(rebarPath='" + myRebarPath + "')";
  }
}
