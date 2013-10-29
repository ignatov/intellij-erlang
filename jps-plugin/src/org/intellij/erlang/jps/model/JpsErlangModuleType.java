package org.intellij.erlang.jps.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.module.JpsModuleType;

/**
 * @author @nik
 */
public class JpsErlangModuleType implements JpsModuleType<JpsDummyElement> {
  public static final JpsErlangModuleType INSTANCE = new JpsErlangModuleType();

  private JpsErlangModuleType() {
  }

  @NotNull
  @Override
  public JpsElementChildRole<JpsDummyElement> getPropertiesRole() {
    return new JpsElementChildRole<JpsDummyElement>();
  }
}
