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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author @nik
 */
public class JpsErlangModelSerializerExtension extends JpsModelSerializerExtension {
  public static final String ERLANG_SDK_TYPE_ID = "Erlang SDK";

  @NotNull
  @Override
  public List<? extends JpsModulePropertiesSerializer<?>> getModulePropertiesSerializers() {
    return Collections.singletonList(new JpsModulePropertiesSerializer<JpsDummyElement>(JpsErlangModuleType.INSTANCE, "ERLANG_MODULE", null) {
      @Override
      public JpsDummyElement loadProperties(@Nullable Element componentElement) {
        return JpsElementFactory.getInstance().createDummyElement();
      }

      @Override
      public void saveProperties(@NotNull JpsDummyElement properties, @NotNull Element componentElement) {
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

      @Override
      public void saveProperties(@NotNull JpsDummyElement properties, @NotNull Element element) {
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
        return new JpsErlangModuleExtension(props == null ? new ErlangModuleExtensionProperties() : props);
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
