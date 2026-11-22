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
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
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

  // the statistics read the cache on a pooled thread while SDK detection writes it on another one
  public void testCacheIsSafeForSeveralThreads() throws Exception {
    ErlangSdkType.ReleaseCache cache = new ErlangSdkType.ReleaseCache(new HashMap<>());
    int threads = 4;
    int homes = 20_000;
    ExecutorService executor = Executors.newFixedThreadPool(threads);
    try {
      List<Future<?>> futures = new ArrayList<>();
      for (int t = 0; t < threads; t++) {
        int thread = t;
        futures.add(executor.submit(() -> {
          for (int i = 0; i < homes; i++) {
            String home = "/sdk/" + thread + "/" + i;
            if (i % 2 == 0) cache.put(home, ErlangSdkRelease.V_27_0);
            else cache.computeIfAbsent(home, h -> ErlangSdkRelease.V_27_0);
            cache.get("/sdk/" + (thread + 1) % threads + "/" + i);
          }
        }));
      }
      for (Future<?> future : futures) future.get(1, TimeUnit.MINUTES);
    }
    finally {
      executor.shutdownNow();
    }
    int missing = 0;
    for (int t = 0; t < threads; t++) {
      for (int i = 0; i < homes; i++) {
        if (cache.get("/sdk/" + t + "/" + i) == null) missing++;
      }
    }
    assertEquals("releases lost", 0, missing);
  }

  // detection may start erl, so a lookup of another home must not wait for it
  public void testDetectionDoesNotBlockTheCache() throws Exception {
    ErlangSdkType.ReleaseCache cache = new ErlangSdkType.ReleaseCache(new HashMap<>());
    cache.put("/sdk/known", ErlangSdkRelease.V_25_0);
    CountDownLatch detecting = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    ExecutorService executor = Executors.newFixedThreadPool(2);
    try {
      Future<ErlangSdkRelease> detection = executor.submit(() -> cache.computeIfAbsent("/sdk/slow", home -> {
        detecting.countDown();
        try {
          release.await(1, TimeUnit.MINUTES);
        }
        catch (InterruptedException e) {
          throw new RuntimeException(e);
        }
        return ErlangSdkRelease.V_27_0;
      }));
      assertTrue(detecting.await(1, TimeUnit.MINUTES));
      Future<ErlangSdkRelease> lookup = executor.submit(() -> cache.get("/sdk/known"));
      assertEquals(ErlangSdkRelease.V_25_0, lookup.get(5, TimeUnit.SECONDS));
      release.countDown();
      assertEquals(ErlangSdkRelease.V_27_0, detection.get(1, TimeUnit.MINUTES));
      assertEquals(ErlangSdkRelease.V_27_0, cache.get("/sdk/slow"));
    }
    finally {
      release.countDown();
      executor.shutdownNow();
    }
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

  public void testKnownReleaseOfProjectSdk() {
    assertEquals("OTP 25, erts-13.0", String.valueOf(ErlangSdkType.getKnownProjectSdkRelease(getProject())));
  }

  public void testKnownReleaseFollowsChangedVersionString() {
    modifyProjectSdk(modificator -> modificator.setVersionString("OTP 27, erts-15.1"));
    assertEquals("OTP 27, erts-15.1", String.valueOf(ErlangSdkType.getKnownProjectSdkRelease(getProject())));
  }

  public void testKnownReleaseOfUnreadableVersionString() {
    modifyProjectSdk(modificator -> modificator.setVersionString("unknown"));
    assertNull(ErlangSdkType.getKnownProjectSdkRelease(getProject()));
  }

  public void testKnownReleaseWithoutProjectSdk() {
    ApplicationManager.getApplication().runWriteAction(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
    try {
      assertNull(ErlangSdkType.getKnownProjectSdkRelease(getProject()));
      assertNull(ErlangSdkType.getProjectSdkRelease(getProject()));
      assertSame(ErlangSdkRelease.DefaultRelease, ErlangSdkType.getRelease(getProject()));
    }
    finally {
      setUpProjectSdk();
    }
  }

  public void testReleaseOfModule() {
    modifyProjectSdk(modificator -> modificator.setVersionString("OTP 27, erts-15.1"));
    assertEquals("27", ErlangSdkType.getRelease(getModule()).getOtpRelease());
    assertSame(ErlangSdkRelease.DefaultRelease, ErlangSdkType.getRelease((Module)null));
  }

  public void testDocumentationUrlFollowsChangedVersionString() {
    assertEquals("https://www.erlang.org/doc", ErlangSdkType.getInstance().getDefaultDocumentationUrl(getProjectSdk()));
    modifyProjectSdk(modificator -> modificator.setVersionString("OTP 28.0.1, erts-16.0.1"));
    assertEquals("https://www.erlang.org/docs/28", ErlangSdkType.getInstance().getDefaultDocumentationUrl(getProjectSdk()));
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
