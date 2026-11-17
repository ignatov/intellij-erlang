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

import com.intellij.util.xmlb.SkipDefaultValuesSerializationFilters;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.rebar.JpsRebarSettingsSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElement;
import org.jetbrains.jps.model.JpsElementFactory;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.serialization.JpsModelSerializerExtension;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;
import org.jetbrains.jps.model.serialization.facet.JpsFacetConfigurationSerializer;
import org.jetbrains.jps.model.serialization.library.JpsSdkPropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModulePropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModuleSourceRootDummyPropertiesSerializer;
import org.jetbrains.jps.model.serialization.module.JpsModuleSourceRootPropertiesSerializer;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class JpsErlangModelSerializerExtension extends JpsModelSerializerExtension {
  public static final String ERLANG_SDK_TYPE_ID = "Erlang SDK";

  @NotNull
  @Override
  public List<? extends JpsModuleSourceRootPropertiesSerializer<?>> getModuleSourceRootPropertiesSerializers() {
    return Collections.singletonList(new JpsModuleSourceRootDummyPropertiesSerializer(ErlangIncludeSourceRootType.INSTANCE, "erlang-include"));
  }

  @NotNull
  @Override
  public List<? extends JpsModulePropertiesSerializer<?>> getModulePropertiesSerializers() {
    return Collections.singletonList(new JpsModulePropertiesSerializer<JpsDummyElement>(JpsErlangModuleType.INSTANCE, "ERLANG_MODULE", null) {
      @Override
      public JpsDummyElement loadProperties(@Nullable Element componentElement) {
        return JpsElementFactory.getInstance().createDummyElement();
      }
    });
  }

  @NotNull
  @Override
  public List<? extends JpsSdkPropertiesSerializer<?>> getSdkPropertiesSerializers() {
    return Collections.singletonList(new JpsSdkPropertiesSerializer<JpsDummyElement>(ERLANG_SDK_TYPE_ID, JpsErlangSdkType.INSTANCE) {
      @NotNull
      @Override
      public JpsDummyElement loadProperties(@Nullable Element propertiesElement) {
        return JpsElementFactory.getInstance().createDummyElement();
      }
    });
  }

  @NotNull
  @Override
  public List<? extends JpsFacetConfigurationSerializer<?>> getFacetConfigurationSerializers() {
    return Collections.singletonList(new JpsFacetConfigurationSerializer<JpsErlangModuleExtension>(JpsErlangModuleExtension.ROLE, ErlangFacetConstants.ID, ErlangFacetConstants.NAME) {
      @Override
      protected JpsErlangModuleExtension loadExtension(@NotNull Element facetConfigurationElement, String name, JpsElement parent, JpsModule module) {
        ErlangModuleExtensionProperties props = XmlSerializer.deserialize(facetConfigurationElement, ErlangModuleExtensionProperties.class);
        return new JpsErlangModuleExtension(props);
      }

      @Override
      protected void saveExtension(JpsErlangModuleExtension extension, Element facetConfigurationTag, JpsModule module) {
        XmlSerializer.serializeInto(extension.getProperties(), facetConfigurationTag, new SkipDefaultValuesSerializationFilters());
      }
    });
  }

  @NotNull
  @Override
  public List<? extends JpsProjectExtensionSerializer> getProjectExtensionSerializers() {
    return Arrays.asList(new JpsRebarSettingsSerializer(), new JpsErlangCompilerOptionsSerializer());
  }
}
