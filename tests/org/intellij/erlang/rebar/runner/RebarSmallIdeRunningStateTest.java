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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;

public class RebarSmallIdeRunningStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public RebarSmallIdeRunningStateTest() {
    super(true);
  }

  public void testRebarCommandLineOnEdtWithoutSdk() throws Exception {
    RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
    configuration.setCommand("compile");
    assertEquals(escriptOnPath(), RebarRunningStateUtil.getRebarCommandLine(configuration).getExePath());
  }

  public void testRebarEunitCommandLineOnEdtWithoutSdk() throws Exception {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "rebar eunit");
    File reportsRoot = new File(FileUtil.getTempDirectory(), "eunit");
    assertEquals(escriptOnPath(), RebarEunitRunningState.getCommandLine(configuration, reportsRoot).getExePath());
  }

  private String escriptOnPath() throws Exception {
    assertTrue(ApplicationManager.getApplication().isDispatchThread());
    assertNull(ErlangSdkType.getSdkPath(getProject()));
    return ApplicationManager.getApplication().executeOnPooledThread(() -> RebarRunningStateUtil.findEscriptExecutable()).get();
  }
}
