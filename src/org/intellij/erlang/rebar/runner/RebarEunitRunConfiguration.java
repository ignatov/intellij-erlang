package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

/**
 * @author savenko
 */
public class RebarEunitRunConfiguration extends RebarRunConfigurationBase {
  protected RebarEunitRunConfiguration(@NotNull Project project, @NotNull String name) {
    super(name, project, RebarEunitRunConfigurationFactory.getInstance());
    setCommand("eunit");
    setSkipDependencies(true);
  }

  @Override
  boolean isUseTestConsole() {
    return true;
  }
}
