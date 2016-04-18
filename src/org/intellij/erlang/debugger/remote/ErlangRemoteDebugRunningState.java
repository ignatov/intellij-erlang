/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.debugger.remote;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.runconfig.ErlangRunningState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ErlangRemoteDebugRunningState extends ErlangRunningState {
  private final ErlangRemoteDebugRunConfiguration myConfiguration;

  public ErlangRemoteDebugRunningState(ExecutionEnvironment env, Module module, ErlangRemoteDebugRunConfiguration configuration) {
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
    return false;
  }

  @Override
  public ErlangEntryPoint getEntryPoint() throws ExecutionException {
    return null;
  }

  @Nullable
  @Override
  public String getWorkDirectory() {
    return null;
  }

  @NotNull
  @Override
  public ConsoleView createConsoleView(Executor executor) {
    TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(getEnvironment().getProject());
    return consoleBuilder.getConsole();
  }

  @Override
  protected List<String> getErlFlags() {
    List<String> result;


    if (myConfiguration.isUseShortNames()) {
      result = new ArrayList<String>(ContainerUtil.list("-sname", getNodeName()));

    } else {
      String host = StringUtil.nullize(myConfiguration.getHost(), true);
      String qualifiedName = getNodeName() + "@" + (host == null ? getDefaultHost() : host);
      result = new ArrayList<String>(ContainerUtil.list("-name", qualifiedName));
    }

    List<String> debugNodeArgs = Arrays.asList(myConfiguration.getDebugNodeArgs().split(" "));
    result.addAll(debugNodeArgs);
    return result;
  }

  @NotNull
  private static String getNodeName() {
    return "debugger_node_" + System.currentTimeMillis();
  }

  @NotNull
  private static String getDefaultHost() {
    try {
      return InetAddress.getLocalHost().getCanonicalHostName();
    } catch (UnknownHostException ignore) {
    }
    return "127.0.0.1";
  }
}
