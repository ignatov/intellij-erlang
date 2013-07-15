package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

/**
 * @author savenko
 */
public class RebarEunitRunConfiguration extends RebarRunConfigurationBase {
  protected RebarEunitRunConfiguration(@NotNull String name, @NotNull Project project) {
    super(name, project, RebarEunitRunConfigurationFactory.getInstance());
  }
}
