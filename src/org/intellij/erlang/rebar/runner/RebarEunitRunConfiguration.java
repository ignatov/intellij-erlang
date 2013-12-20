package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

public class RebarEunitRunConfiguration extends RebarRunConfigurationBase {
  protected RebarEunitRunConfiguration(@NotNull Project project, @NotNull String name) {
    super(name, project, RebarEunitRunConfigurationFactory.getInstance());
    setCommand("eunit");
    setSkipDependencies(true);
  }

  @NotNull
  @Override
  public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
    return new RebarEunitRunningState(env, this);
  }
}