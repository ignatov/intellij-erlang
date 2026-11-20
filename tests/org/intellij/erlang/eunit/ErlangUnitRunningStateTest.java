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

package org.intellij.erlang.eunit;

import com.intellij.execution.ExecutionException;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.runconfig.ErlangRunningState;

import java.util.Set;

public class ErlangUnitRunningStateTest extends BasePlatformTestCase {
  public void testFunctionNameWithoutModuleDoesNotThrow() {
    doTestMalformedFunctionName("my_test");
  }

  public void testFunctionNameWithTrailingColonDoesNotThrow() {
    doTestMalformedFunctionName("my_tests:");
  }

  private void doTestMalformedFunctionName(String functionName) {
    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(getProject(), "eunit", ErlangUnitRunConfigurationType.getInstance());
    configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
    configuration.getConfigData().setFunctionNames(Set.of(functionName));
    ErlangUnitRunningState state = new ErlangUnitRunningState(null, getModule(), configuration);
    try {
      ErlangRunningState.ErlangEntryPoint entryPoint = state.getEntryPoint();
      assertEquals("eunit", entryPoint.getModuleName());
      assertEquals("test", entryPoint.getFunctionName());
      assertFalse(String.valueOf(entryPoint.getArgsList()), entryPoint.getArgsList().get(0).contains("my_test"));
    }
    catch (ExecutionException e) {
      assertTrue(e.getMessage(), e.getMessage().contains(functionName));
    }
  }
}
