package org.intellij.erlang.jps.model;

import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.module.JpsModuleType;

/**
 * @author @nik
 */
public class JpsErlangModuleType extends JpsModuleType<JpsDummyElement> {
  public static final JpsErlangModuleType INSTANCE = new JpsErlangModuleType();

  private JpsErlangModuleType() {
  }
}
