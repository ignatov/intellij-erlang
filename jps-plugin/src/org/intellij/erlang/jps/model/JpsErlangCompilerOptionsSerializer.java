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
    JpsErlangCompilerOptionsExtension extension = JpsErlangCompilerOptionsExtension.getOrCreateExtension(project);
    ErlangCompilerOptions options = XmlSerializer.deserialize(componentTag, ErlangCompilerOptions.class);
    if (options != null) {
      extension.setOptions(options);
    }
  }

  @Override
  public void saveExtension(@NotNull JpsProject project, @NotNull Element componentTag) {
  }
}
