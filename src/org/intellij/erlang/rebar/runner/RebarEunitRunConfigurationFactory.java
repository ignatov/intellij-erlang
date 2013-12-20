package org.intellij.erlang.rebar.runner;

import com.intellij.compiler.options.CompileStepBeforeRun;
import com.intellij.execution.BeforeRunTask;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;

public class RebarEunitRunConfigurationFactory extends ConfigurationFactory {
  private static final RebarEunitRunConfigurationFactory ourInstance = new RebarEunitRunConfigurationFactory();

  private RebarEunitRunConfigurationFactory() {
    super(RebarEunitRunConfigurationType.getInstance());
  }

  @Override
  public void configureBeforeRunTaskDefaults(Key<? extends BeforeRunTask> providerID, BeforeRunTask task) {
    if (providerID == CompileStepBeforeRun.ID) {
      task.setEnabled(false);
    }
  }

  @NotNull
  public static RebarEunitRunConfigurationFactory getInstance() {
    return ourInstance;
  }

  @Override
  public RunConfiguration createTemplateConfiguration(@NotNull Project project) {
    return new RebarEunitRunConfiguration(project, "Erlang Rebar Eunit");
  }
}
