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

package org.intellij.erlang.jps.rebar;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;

public class JpsRebarConfigurationExtension extends JpsCompositeElementBase<JpsRebarConfigurationExtension> {
  private static final JpsElementChildRole<JpsRebarConfigurationExtension> ROLE = JpsElementChildRoleBase.create("Rebar");

  private RebarSettingsState myState;

  private JpsRebarConfigurationExtension(RebarSettingsState state) {
    myState = state;
  }

  @NotNull
  @Override
  public JpsRebarConfigurationExtension createCopy() {
    return new JpsRebarConfigurationExtension(new RebarSettingsState(myState));
  }

  public void setRebarSettingsState(RebarSettingsState state) {
    myState = state;
  }

  public RebarSettingsState getRebarSettingsState() {
    return myState;
  }

  public String getRebarPath() {
    return myState.myRebarPath;
  }

  @Nullable
  public static JpsRebarConfigurationExtension getExtension(@Nullable JpsProject project) {
    return null != project ? project.getContainer().getChild(ROLE) : null;
  }

  @NotNull
  public static JpsRebarConfigurationExtension getOrCreateExtension(@NotNull JpsProject project) {
    JpsRebarConfigurationExtension extension = getExtension(project);
    if (extension == null) {
      extension = project.getContainer().setChild(ROLE, new JpsRebarConfigurationExtension(new RebarSettingsState()));
    }
    return extension;
  }
}
