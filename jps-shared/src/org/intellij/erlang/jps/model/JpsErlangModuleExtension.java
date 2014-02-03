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

  public JpsErlangModuleExtension(JpsErlangModuleExtension moduleExtension) {
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

  @Nullable
  public static JpsErlangModuleExtension getExtension(@Nullable JpsModule module) {
    return module != null ? module.getContainer().getChild(ROLE) : null;
  }
}
