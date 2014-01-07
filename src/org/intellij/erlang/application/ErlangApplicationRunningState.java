/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.application;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangApplicationRunningState extends ErlangRunningState {
  private ErlangApplicationConfiguration myConfiguration;

  public ErlangApplicationRunningState(ExecutionEnvironment env, Module module, ErlangApplicationConfiguration configuration) {
    super(env, module);
    myConfiguration = configuration;
  }

  @Override
  protected boolean useTestCodePath() {
    return false;
  }

  @Override
  protected boolean isNoShellMode() {
    return true;
  }

  @Override
  protected boolean isStopErlang() {
    return myConfiguration.stopErlang();
  }

  @Override
  protected List<String> getErlFlags() {
    return StringUtil.split(myConfiguration.getErlFlags(), " ");
  }

  @Nullable
  @Override
  public ErlangEntryPoint getEntryPoint() throws ExecutionException {
    ErlangEntryPoint entryPoint = ErlangEntryPoint.fromModuleAndFunction(myConfiguration.getModuleAndFunction(), myConfiguration.getParams());
    if (entryPoint == null) {
      throw new ExecutionException("Invalid entry point");
    }
    return entryPoint;
  }

  @NotNull
  @Override
  public ConsoleView createConsoleView(Executor executor) {
    final TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(myConfiguration.getProject());
    return consoleBuilder.getConsole();
  }
}
