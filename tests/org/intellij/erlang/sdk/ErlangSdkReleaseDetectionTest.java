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

package org.intellij.erlang.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

// An SDK home without releases/*/OTP_VERSION, erts-* or start_erl.data, so the release can only be found by running erl.
public class ErlangSdkReleaseDetectionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private File mySdkHome;
  private File myErlRuns;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    mySdkHome = FileUtil.createTempDirectory("erlang-sdk-without-version-files", null);
    myErlRuns = new File(mySdkHome, "erl-runs");
    File bin = new File(mySdkHome, "bin");
    assertTrue(bin.mkdirs());
    writeExecutable(new File(bin, "erl"),
                    "#!/bin/sh\n" +
                    "echo run >> '" + myErlRuns.getAbsolutePath() + "'\n" +
                    "printf '\\nErlangSdkType_OTP_RELEASE:\\n27\\nErlangSdkType_ERTS_VERSION:\\n15.2\\n'\n");
    writeExecutable(new File(bin, "erlc"), "#!/bin/sh\n");
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      FileUtil.delete(mySdkHome);
    }
    finally {
      super.tearDown();
    }
  }

  // #985: the release is asked for under a read action (editor notification, inspections) and on the EDT;
  // finding it must not start an erl process there.
  public void testReleaseOfFileDoesNotRunErlUnderReadAction() {
    if (SystemInfo.isWindows) return;
    setProjectSdk(mySdkHome.getAbsolutePath());
    myFixture.configureByText("a.erl", "-module(a).\n");

    ReadAction.compute(() -> ErlangSdkType.getRelease(myFixture.getFile()));

    assertFalse("erl was run under a read action on " + Thread.currentThread().getName(), myErlRuns.exists());
  }

  private void setProjectSdk(String sdkHome) {
    ApplicationManager.getApplication().runWriteAction(() -> {
      Sdk sdk = new ProjectJdkImpl("Erlang without version files", ErlangSdkType.getInstance());
      var modificator = sdk.getSdkModificator();
      modificator.setHomePath(sdkHome);
      modificator.commitChanges();
      ProjectJdkTable.getInstance().addJdk(sdk);
      ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
    });
  }

  private static void writeExecutable(File file, String text) throws Exception {
    Files.writeString(file.toPath(), text, StandardCharsets.UTF_8);
    assertTrue(file.setExecutable(true));
  }
}
