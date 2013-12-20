package org.intellij.erlang.rebar.runner;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.openapi.extensions.Extensions;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NotNull;

public class RebarEunitRunConfigurationType extends ConfigurationTypeBase {
  RebarEunitRunConfigurationType() {
    super("RebarEunitRunConfigurationType", "Erlang Rebar Eunit", "Runs Eunit tests with Rebar", ErlangIcons.REBAR_EUNIT);
  }

  public static RebarEunitRunConfigurationType getInstance() {
    return Extensions.findExtension(CONFIGURATION_TYPE_EP, RebarEunitRunConfigurationType.class);
  }

  @NotNull
  @Override
  public ConfigurationFactory[] getConfigurationFactories() {
    return new ConfigurationFactory[]{RebarEunitRunConfigurationFactory.getInstance()};
  }
}
