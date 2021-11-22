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

package org.intellij.erlang.emacs;

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

@State(name = "EmacsSettings", storages = {@Storage(value = "emacs.xml")})
public final class EmacsSettings implements PersistentStateComponent<EmacsSettings> {
  @NotNull
  private String myEmacsPath = "";

  @NotNull
  public static EmacsSettings getInstance(@NotNull Project project) {
    EmacsSettings persisted = project.getService(EmacsSettings.class);
    return persisted != null ? persisted : new EmacsSettings();
  }

  @NotNull
  @Override
  public EmacsSettings getState() {
    return this;
  }

  @Override
  public void loadState(@NotNull EmacsSettings emacsSettings) {
    XmlSerializerUtil.copyBean(emacsSettings, this);
  }

  @NotNull
  public String getEmacsPath() {
    return myEmacsPath;
  }

  public void setEmacsPath(@NotNull String emacsPath) {
    myEmacsPath = emacsPath;
  }

  @Override
  public String toString() {
    return "EmacsSettings(emacsPath='" + myEmacsPath + "')";
  }
}