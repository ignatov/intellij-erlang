package org.intellij.erlang.eunit;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.ErlangIcons;

/**
 * @author ignatov
 */
public class ErlangUnitRunConfigurationType extends ConfigurationTypeBase {
  public static final String PROTOCOL = "enuit";

  protected ErlangUnitRunConfigurationType() {
    super("ErlangUnitRunConfigurationType",
          "Eunit",
          "Eunit run configuration",
      ErlangIcons.EUNIT);
    addFactory(new ErlangUnitConfigurationFactory(this));
  }

  public static ErlangUnitRunConfigurationType getInstance() {
    return Extensions.findExtension(CONFIGURATION_TYPE_EP, ErlangUnitRunConfigurationType.class);
  }

  public static class ErlangUnitConfigurationFactory extends ConfigurationFactory {
    protected ErlangUnitConfigurationFactory(ErlangUnitRunConfigurationType type) {
      super(type);
    }

    @Override
    public RunConfiguration createTemplateConfiguration(Project project) {
      return new ErlangUnitRunConfiguration(project, "Erlang", getInstance());
    }
  }
}
