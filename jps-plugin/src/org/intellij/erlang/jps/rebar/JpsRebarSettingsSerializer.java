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

import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;

public class JpsRebarSettingsSerializer extends JpsProjectExtensionSerializer {
  public static final String REBAR_CONFIG_FILE_NAME = "rebar.xml";
  public static final String REBAR_COMPONENT_NAME = "RebarSettings";

  public JpsRebarSettingsSerializer() {
    super(REBAR_CONFIG_FILE_NAME, REBAR_COMPONENT_NAME);
  }

  @Override
  public void loadExtension(@NotNull JpsProject jpsProject, @NotNull Element componentTag) {
    JpsRebarConfigurationExtension extension = JpsRebarConfigurationExtension.getOrCreateExtension(jpsProject);
    RebarSettingsState rebarSettings = XmlSerializer.deserialize(componentTag, RebarSettingsState.class);
    extension.setRebarSettingsState(rebarSettings);
  }
}
