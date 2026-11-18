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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.application.impl.LaterInvocator;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.rebar.settings.RebarConfigurationForm;

import java.io.File;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

public class RebarProjectRootStepTest extends BasePlatformTestCase {
  // The wizard builds its steps before it shows its modal dialog. The rebar found in the project directory must be
  // filled in while that dialog is open, not after it has closed.
  public void testDetectedRebarPathArrivesWhileWizardIsModal() throws Exception {
    File directory = FileUtil.createTempDirectory("rebarRootStep", null, true);
    File rebar = new File(directory, "rebar3");
    Files.writeString(rebar.toPath(), "#!/bin/sh\necho rebar 3.0.0\n", StandardCharsets.UTF_8);
    assertTrue(rebar.setExecutable(true));

    WizardContext context = new WizardContext(getProject(), getTestRootDisposable());
    context.setProjectFileDirectory(directory.getPath());
    RebarProjectRootStep step = new RebarProjectRootStep(context);
    RebarConfigurationForm form = getForm(step);

    Object modalEntity = new Object();
    LaterInvocator.enterModal(modalEntity);
    try {
      long deadline = System.currentTimeMillis() + 10000;
      while (!rebar.getPath().equals(form.getPath()) && System.currentTimeMillis() < deadline) {
        PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
        PlatformTestUtil.waitForAlarm(10);
      }
      assertEquals(rebar.getPath(), form.getPath());
    }
    finally {
      LaterInvocator.leaveModal(modalEntity);
    }
  }

  private static RebarConfigurationForm getForm(RebarProjectRootStep step) throws ReflectiveOperationException {
    Field field = RebarProjectRootStep.class.getDeclaredField("myRebarConfigurationForm");
    field.setAccessible(true);
    return (RebarConfigurationForm) field.get(step);
  }
}
