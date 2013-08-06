package org.intellij.erlang.jps.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;

/**
 * @author savenko
 */
public class JpsErlangCompilerOptionsExtension extends JpsCompositeElementBase<JpsErlangCompilerOptionsExtension> {
  public static final JpsElementChildRole<JpsErlangCompilerOptionsExtension> ROLE = JpsElementChildRoleBase.create("ErlangCompilerOptions");

  private ErlangCompilerOptions myOptions;


  public JpsErlangCompilerOptionsExtension(ErlangCompilerOptions options) {
    myOptions = options;
  }

  @NotNull
  @Override
  public JpsErlangCompilerOptionsExtension createCopy() {
    return new JpsErlangCompilerOptionsExtension(new ErlangCompilerOptions(myOptions));
  }

  public ErlangCompilerOptions getOptions() {
    return myOptions;
  }

  public void setOptions(ErlangCompilerOptions options) {
    myOptions = options;
  }

  @NotNull
  public static JpsErlangCompilerOptionsExtension getOrCreateExtension(@NotNull JpsProject project) {
    JpsErlangCompilerOptionsExtension extension = project.getContainer().getChild(ROLE);
    if (extension == null) {
      extension = project.getContainer().setChild(ROLE, new JpsErlangCompilerOptionsExtension(new ErlangCompilerOptions()));
    }
    return extension;
  }
}
