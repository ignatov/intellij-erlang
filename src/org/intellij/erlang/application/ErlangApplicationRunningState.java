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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.runconfig.ErlangRunningState;

/**
 * @author ignatov
 */
public class ErlangApplicationRunningState extends ErlangRunningState {
  private ErlangApplicationConfiguration myConfiguration;

  public ErlangApplicationRunningState(ExecutionEnvironment env, Module module, ErlangApplicationConfiguration configuration) {
    super(env, module);
    myConfiguration = configuration;
  }

  @Override
  protected void setUpCommandLineParameters(GeneralCommandLine commandLine) {
    commandLine.addParameters("-run");
    commandLine.addParameters(StringUtil.split(myConfiguration.getModuleAndFunction(), " "));
    commandLine.addParameters(StringUtil.split(myConfiguration.getParams(), " "));
    commandLine.addParameters("-noshell");
    if (myConfiguration.stopErlang()) {
      commandLine.addParameters("-s", "init", "stop");
    }
  }

  @Override
  protected boolean useTestCodePath() {
    return false;
  }
}
