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

package org.intellij.erlang.jps.model;

import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;

public class JpsErlangCompilerOptionsSerializer extends JpsProjectExtensionSerializer {
  public static final String COMPILER_OPTIONS_COMPONENT_NAME = "ErlangCompilerOptions";

  public JpsErlangCompilerOptionsSerializer() {
    super("compiler.xml", COMPILER_OPTIONS_COMPONENT_NAME);
  }

  @Override
  public void loadExtension(@NotNull JpsProject project, @NotNull Element componentTag) {
    ErlangCompilerOptions options = XmlSerializer.deserialize(componentTag, ErlangCompilerOptions.class);
    project.getContainer().setChild(JpsErlangCompilerOptionsExtension.ROLE, new JpsErlangCompilerOptionsExtension(options));
  }
}
