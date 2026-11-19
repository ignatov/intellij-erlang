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
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.function.Consumer;

// #1300: the release of an Erlang SDK must follow a change of its version string or of its home.
public class ErlangSdkTypeReleaseCacheTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R25/", ErlangSdkRelease.V_25_0);
      }
    };
  }

  public void testReleaseFollowsChangedVersionString() {
    assertEquals("25", ErlangSdkType.getRelease(getProject()).getOtpRelease());

    modifyProjectSdk(modificator -> modificator.setVersionString("OTP 27, erts-15.1"));

    assertEquals("OTP 27, erts-15.1", getProjectSdk().getVersionString());
    assertEquals("27", ErlangSdkType.getRelease(getProject()).getOtpRelease());
  }

  public void testReleaseFollowsChangedHome() {
    assertEquals("25", ErlangSdkType.getRelease(getProject()).getOtpRelease());

    ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0); // pre-caches the release of that home
    modifyProjectSdk(modificator -> {
      modificator.setHomePath("testData/mockSdk-R27/");
      modificator.setVersionString(ErlangSdkRelease.V_27_0.toString());
    });

    assertEquals("27", ErlangSdkType.getRelease(getProject()).getOtpRelease());
  }

  // Project Structure changes only the home and keeps the version string of the old home
  public void testReleaseFollowsChangedHomeAlone() {
    assertEquals("25", ErlangSdkType.getRelease(getProject()).getOtpRelease());

    ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0); // pre-caches the release of that home
    modifyProjectSdk(modificator -> modificator.setHomePath("testData/mockSdk-R27/"));

    assertEquals("27", ErlangSdkType.getRelease(getProject()).getOtpRelease());
    String expectedVersionString = ErlangSdkRelease.V_27_0.toString();
    PlatformTestUtil.waitWithEventsDispatching(() -> "the version string is " + getProjectSdk().getVersionString(),
                                               () -> expectedVersionString.equals(getProjectSdk().getVersionString()), 10);
    assertEquals("27", ErlangSdkType.getRelease(getProject()).getOtpRelease());
  }

  public void testReleaseFollowsChangedHomeWithoutVersionString() {
    modifyProjectSdk(modificator -> modificator.setVersionString(null));
    assertEquals("25", ErlangSdkType.getRelease(getProject()).getOtpRelease());

    ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0); // pre-caches the release of that home
    modifyProjectSdk(modificator -> modificator.setHomePath("testData/mockSdk-R27/"));

    assertEquals("27", ErlangSdkType.getRelease(getProject()).getOtpRelease());
  }

  @NotNull
  private Sdk getProjectSdk() {
    Sdk sdk = ProjectRootManager.getInstance(getProject()).getProjectSdk();
    assertNotNull(sdk);
    return sdk;
  }

  private void modifyProjectSdk(@NotNull Consumer<SdkModificator> change) {
    Sdk sdk = getProjectSdk();
    ApplicationManager.getApplication().runWriteAction(() -> {
      SdkModificator modificator = sdk.getSdkModificator();
      change.accept(modificator);
      modificator.commitChanges();
    });
  }
}
