package org.intellij.erlang.jps.rebar;

import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.serialization.JpsProjectExtensionSerializer;

/**
 * @author savenko
 */
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
    if (rebarSettings != null) {
      extension.setRebarSettingsState(rebarSettings);
    }
  }

  @Override
  public void saveExtension(@NotNull JpsProject jpsProject, @NotNull Element componentTag) {
  }
}
