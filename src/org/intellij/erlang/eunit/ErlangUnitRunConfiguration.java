package org.intellij.erlang.eunit;

import com.intellij.execution.configurations.*;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.eunit.ui.ErlangUnitRunConfigurationEditorForm;
import org.intellij.erlang.runner.ErlangApplicationConfiguration;
import org.intellij.erlang.runner.ErlangRunConfigurationType;

/**
 * @author ignatov
 */
public class ErlangUnitRunConfiguration extends ErlangApplicationConfiguration {
  public ErlangUnitRunConfiguration(Project project, String name, ErlangUnitRunConfigurationType configurationType) {
    super(project, name, configurationType);
  }

  @Override
  public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
    return new ErlangUnitRunConfigurationEditorForm();
  }

  @Override
  protected ModuleBasedConfiguration createInstance() {
    return new ErlangUnitRunConfiguration(getProject(), getName(), ErlangUnitRunConfigurationType.getInstance());
  }

  @Override
  public boolean isGeneratedName() {
    return "Unnamed".equals(getName());
  }

  @Override
  public String suggestedName() {
    return "Unnamed";
  }
}
