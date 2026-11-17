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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;

// The run configurations are started on the EDT (DefaultProgramRunner -> ExecutionManager), as the tests run here.
public class RebarRunningStateWithoutProjectSdkTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private Sdk myOldProjectSdk;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ProjectRootManager rootManager = ProjectRootManager.getInstance(getProject());
    myOldProjectSdk = rootManager.getProjectSdk();
    ApplicationManager.getApplication().runWriteAction(() -> rootManager.setProjectSdk(null));
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ApplicationManager.getApplication().runWriteAction(
        () -> ProjectRootManager.getInstance(getProject()).setProjectSdk(myOldProjectSdk));
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testRebarCommandLineFallsBackToEscriptOnPath() {
    assertNull(ErlangSdkType.getSdkPath(getProject()));
    RebarRunConfiguration configuration = new RebarRunConfiguration("rebar", getProject());
    configuration.setCommand("compile");
    assertEscript(RebarRunningStateUtil.getRebarCommandLine(configuration));
  }

  public void testRebarEunitCommandLineFallsBackToEscriptOnPath() {
    assertNull(ErlangSdkType.getSdkPath(getProject()));
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(getProject(), "eunit");
    assertEscript(RebarEunitRunningState.getCommandLine(configuration, new File(FileUtil.getTempDirectory(), "eunit")));
  }

  private static void assertEscript(GeneralCommandLine commandLine) {
    String exe = commandLine.getExePath();
    String expectedName = JpsErlangSdkType.getExecutableFileName(JpsErlangSdkType.SCRIPT_INTERPRETER);
    assertTrue(exe, exe.equals(expectedName) || new File(exe).getName().equals(expectedName));
  }
}
