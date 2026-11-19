/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ExecutionEnvironmentBuilder;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.openapi.util.Ref;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

public class RebarRunnerTest extends BasePlatformTestCase {
  public void testRunsRebarConfigurationsWithRunExecutorOnly() {
    RebarRunConfiguration rebar = new RebarRunConfiguration("rebar", getProject());
    RebarEunitRunConfiguration eunit = new RebarEunitRunConfiguration(getProject(), "eunit");

    assertInstanceOf(ProgramRunner.findRunnerById("RebarRunner"), RebarRunner.class);
    assertInstanceOf(ProgramRunner.getRunner(DefaultRunExecutor.EXECUTOR_ID, rebar), RebarRunner.class);
    assertInstanceOf(ProgramRunner.getRunner(DefaultRunExecutor.EXECUTOR_ID, eunit), RebarRunner.class);
    assertFalse(new RebarRunner().canRun(DefaultDebugExecutor.EXECUTOR_ID, rebar));
  }

  public void testExecutesStateWithItself() throws ExecutionException {
    RebarRunner runner = new RebarRunner();
    ExecutionEnvironment environment = ExecutionEnvironmentBuilder
      .create(getProject(), DefaultRunExecutor.getRunExecutorInstance(), new RebarRunConfiguration("rebar", getProject()))
      .runner(runner)
      .build();
    Ref<ProgramRunner<?>> executedBy = new Ref<>();
    RunProfileState state = (executor, programRunner) -> {
      executedBy.set(programRunner);
      return null;
    };

    assertNull(runner.doExecute(state, environment));
    assertSame(runner, executedBy.get());
  }
}
