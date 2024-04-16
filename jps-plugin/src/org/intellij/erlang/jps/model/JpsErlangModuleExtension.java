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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;
import org.jetbrains.jps.model.module.JpsModule;

import java.util.Collections;
import java.util.List;

public class JpsErlangModuleExtension extends JpsCompositeElementBase<JpsErlangModuleExtension> {
  public static final JpsElementChildRole<JpsErlangModuleExtension> ROLE = JpsElementChildRoleBase.create("Erlang");

  private final ErlangModuleExtensionProperties myProperties;

  @SuppressWarnings("UnusedDeclaration")
  public JpsErlangModuleExtension() {
    myProperties = new ErlangModuleExtensionProperties();
  }

  public JpsErlangModuleExtension(ErlangModuleExtensionProperties properties) {
    myProperties = properties;
  }

  private JpsErlangModuleExtension(JpsErlangModuleExtension moduleExtension) {
    myProperties = new ErlangModuleExtensionProperties(moduleExtension.myProperties);
  }

  @NotNull
  @Override
  public JpsErlangModuleExtension createCopy() {
    return new JpsErlangModuleExtension(this);
  }

  public ErlangModuleExtensionProperties getProperties() {
    return myProperties;
  }

  public List<String> getParseTransforms() {
    return Collections.unmodifiableList(myProperties.myParseTransforms);
  }

  public List<String> getExtraFlags() {
    return Collections.unmodifiableList(myProperties.myExtraFlags);
  }

  @Nullable
  public static JpsErlangModuleExtension getExtension(@Nullable JpsModule module) {
    return module != null ? module.getContainer().getChild(ROLE) : null;
  }
}
