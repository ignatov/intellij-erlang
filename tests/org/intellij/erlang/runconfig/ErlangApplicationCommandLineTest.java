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

package org.intellij.erlang.runconfig;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.application.ErlangApplicationRunConfigurationType;
import org.intellij.erlang.application.ErlangApplicationRunningState;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Method;
import java.util.List;

public class ErlangApplicationCommandLineTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  // #616: the user's "Flags for 'erl'" must run before the entry point, as `erl` runs -s and -eval in command-line order
  public void testErlFlagsPrecedeEntryPoint() throws Exception {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModuleAndFunction("riak_dt_runner boot");
    configuration.setErlFlags("-s lager start -s creator_app start -config app");

    List<String> params = getCommandParameters(new ErlangApplicationRunningState(null, getModule(), configuration));

    int eval = params.indexOf("-eval");
    assertTrue(params.toString(), eval >= 0);
    assertEquals(params.toString(), "riak_dt_runner:boot().", params.get(eval + 1));
    int lager = params.indexOf("lager");
    assertTrue(params.toString(), lager >= 0);
    assertTrue("'-s lager start' must come before '-eval': " + params, lager < eval);
    assertTrue("'-config' must come before '-eval': " + params, params.indexOf("-config") < eval);
    assertTrue("'-s init stop' must come after '-eval': " + params, params.indexOf("init") > eval);
  }

  // everything after -extra is plain arguments, so -extra and what follows it must stay at the end
  public void testExtraArgumentsStayLast() throws Exception {
    ErlangApplicationConfiguration configuration =
      new ErlangApplicationConfiguration(getProject(), "app", ErlangApplicationRunConfigurationType.getInstance());
    configuration.setModuleAndFunction("riak_dt_runner boot");
    configuration.setErlFlags("-s lager start -extra foo bar");

    List<String> params = getCommandParameters(new ErlangApplicationRunningState(null, getModule(), configuration));

    assertEquals(List.of("-pa", "/src", "-s", "lager", "start", "-eval", "riak_dt_runner:boot().", "-s", "init", "stop",
                         "-noshell", "-extra", "foo", "bar"), params);
  }

  private static List<String> getCommandParameters(ErlangRunningState state) throws Exception {
    Method getCommand = ErlangRunningState.class.getDeclaredMethod("getCommand");
    getCommand.setAccessible(true);
    GeneralCommandLine commandLine = (GeneralCommandLine)getCommand.invoke(state);
    return commandLine.getParametersList().getList();
  }
}
