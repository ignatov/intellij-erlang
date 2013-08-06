package org.intellij.erlang.jps.rebar;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;

/**
 * @author savenko
 */
public class JpsRebarConfigurationExtension extends JpsCompositeElementBase<JpsRebarConfigurationExtension> {
  public static final JpsElementChildRole<JpsRebarConfigurationExtension> ROLE = JpsElementChildRoleBase.create("Rebar");

  private RebarSettingsState myState;

  public JpsRebarConfigurationExtension(RebarSettingsState state) {
    myState = state;
  }

  @NotNull
  @Override
  public JpsRebarConfigurationExtension createCopy() {
    return new JpsRebarConfigurationExtension(new RebarSettingsState(myState));
  }

  public void setRebarSettingsState(RebarSettingsState state) {
    myState = state;
  }

  public RebarSettingsState getRebarSettingsState() {
    return myState;
  }

  public String getRebarPath() {
    return myState.myRebarPath;
  }

  @Nullable
  public static JpsRebarConfigurationExtension getExtension(@Nullable JpsProject project) {
    return null != project ? project.getContainer().getChild(ROLE) : null;
  }

  @NotNull
  public static JpsRebarConfigurationExtension getOrCreateExtension(@NotNull JpsProject project) {
    JpsRebarConfigurationExtension extension = getExtension(project);
    if (extension == null) {
      extension = project.getContainer().setChild(ROLE, new JpsRebarConfigurationExtension(new RebarSettingsState()));
    }
    return extension;
  }
}
