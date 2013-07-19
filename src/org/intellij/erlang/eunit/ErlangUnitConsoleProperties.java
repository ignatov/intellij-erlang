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

package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangUnitConsoleProperties extends SMTRunnerConsoleProperties implements SMCustomMessagesParsing {

  private RuntimeConfiguration myConfig;

  public ErlangUnitConsoleProperties(final RuntimeConfiguration config, final Executor executor) {
    super(config, "Erlang", executor);
    myConfig = config;
  }

  @Override
  public RuntimeConfiguration getConfiguration() {
    return myConfig;
  }

  @Override
  public OutputToGeneralTestEventsConverter createTestEventsConverter(@NotNull final String testFrameworkName, @NotNull final TestConsoleProperties consoleProperties) {
    return new ErlangUnitTestEventsConverter(testFrameworkName, consoleProperties);
  }
}