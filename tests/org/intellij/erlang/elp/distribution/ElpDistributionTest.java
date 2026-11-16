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

package org.intellij.erlang.elp.distribution;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.NioFiles;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.sun.net.httpserver.HttpServer;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServerSource;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The public entry point for the settings and the offer: whether a project on OTP 28 has a server or can download one, the
 * download with its callback, and the order in which the server path is resolved. The machine is taken for macOS arm64; the
 * tarball is a small fake one served by a local HTTP server.
 */
public class ElpDistributionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String FILE = "elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz";

  private ElpSettings.ElpSettingsState myOldState;
  private String myOldPathProperty;
  private HttpServer myServer;
  private final AtomicInteger myRequests = new AtomicInteger();
  private final AtomicInteger myChanges = new AtomicInteger();
  private byte[] myTarball;
  private Path myBaseDir;
  private ElpInstaller myInstaller;
  private ElpRelease.Asset myAsset;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R28/", ErlangSdkRelease.V_28_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.source = old.source;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
    myOldPathProperty = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);
    store(ElpServerSource.DOWNLOADED, "", ElpMode.ELP);

    myTarball = ElpInstallerTest.tarGz("elp", ElpInstallerTest.FAKE_ELP);
    myServer = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    myServer.createContext("/dl/", exchange -> {
      myRequests.incrementAndGet();
      try (exchange) {
        exchange.sendResponseHeaders(200, myTarball.length);
        try (OutputStream out = exchange.getResponseBody()) {
          out.write(myTarball);
        }
      }
    });
    myServer.start();
    myBaseDir = Files.createTempDirectory("erlang-elp-distribution-test");
    setUpRelease(ElpInstallerTest.sha256(myTarball));
    ApplicationManager.getApplication().getMessageBus().connect(getTestRootDisposable())
      .subscribe(ElpSettings.TOPIC, (ElpSettings.Listener)myChanges::incrementAndGet);
  }

  /** A release with the OTP 27 and 28 builds for macOS arm64, both served as the fake tarball. */
  private void setUpRelease(String sha256) {
    myAsset = new ElpRelease.Asset(FILE, "macos", "aarch64", "aarch64-apple-darwin", "28.5", myTarball.length, sha256, "elp");
    ElpRelease.Asset otp27 = new ElpRelease.Asset("elp-macos-aarch64-apple-darwin-otp-27.3.tar.gz", "macos", "aarch64",
                                                  "aarch64-apple-darwin", "27.3", myTarball.length, sha256, "elp");
    ElpRelease release = new ElpRelease("test-tag", "1.1.0", "https://example.invalid/repo", "https://example.invalid/dl/",
                                        "MIT OR Apache-2.0", List.of(otp27, myAsset));
    myInstaller = new ElpInstaller(myBaseDir, release, "http://127.0.0.1:" + myServer.getAddress().getPort() + "/dl/",
                                   (executable, r, indicator) -> {});
    ElpDistribution.getInstance().setUpForTests(myInstaller, new ElpPlatform("macos", "aarch64"), getTestRootDisposable());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
      if (myOldPathProperty == null) System.clearProperty(ElpConfiguration.PATH_PROPERTY);
      else System.setProperty(ElpConfiguration.PATH_PROPERTY, myOldPathProperty);
      if (myServer != null) myServer.stop(0);
      if (myBaseDir != null) NioFiles.deleteRecursively(myBaseDir);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testDownloadMakesTheServerAvailable() throws Exception {
    ElpAvailability before = background(() -> ElpDistribution.getInstance().availability(getProject()));
    assertEquals(ElpAvailability.Kind.DOWNLOADABLE, before.kind());
    assertFalse(before.hasServer());
    assertEquals(myAsset, before.choice().asset());
    assertEquals(myTarball.length, before.choice().asset().size());
    assertEquals("", effectivePath());
    assertEquals("nothing is downloaded without download()", 0, myRequests.get());

    Outcome first = new Outcome();
    ElpDistribution.getInstance().download(getProject(), myAsset, first);
    first.await();
    Path elp = myInstaller.executable(myAsset);
    assertEquals(List.of("installed " + elp), first.events);
    assertEquals(1, myRequests.get());
    assertFalse(ElpDistribution.getInstance().isDownloading(myAsset));

    ElpAvailability after = background(() -> ElpDistribution.getInstance().availability(getProject()));
    assertEquals(ElpAvailability.Kind.INSTALLED, after.kind());
    assertEquals(elp.toString(), after.serverPath());
    assertEquals(elp.toString(), effectivePath());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals("the servers are told that a binary arrived", 1, myChanges.get());

    Outcome again = new Outcome();
    ElpDistribution.getInstance().download(getProject(), myAsset, again);
    again.await();
    assertEquals(List.of("installed " + elp), again.events);
    assertEquals("an installed build is not downloaded again", 1, myRequests.get());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals("nothing new, nothing to restart", 1, myChanges.get());
  }

  public void testFailedDownloadIsReported() throws Exception {
    setUpRelease("0".repeat(64));
    Outcome outcome = new Outcome();
    ElpDistribution.getInstance().download(getProject(), myAsset, outcome);
    outcome.await();
    assertEquals(1, outcome.events.size());
    String event = outcome.events.get(0);
    assertTrue(event, event.startsWith("failed The ELP download from "));
    assertTrue(event, event.contains("expected sha256 " + "0".repeat(64) + ", got " + ElpInstallerTest.sha256(myTarball)));
    assertEquals(ElpAvailability.Kind.DOWNLOADABLE, background(() -> ElpDistribution.getInstance().availability(getProject())).kind());
    PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
    assertEquals(0, myChanges.get());
  }

  /** The property, then the custom path, then the installed download; the property wins even when it is empty. */
  public void testServerPathOrder() throws Exception {
    Path elp = myInstaller.executable(myAsset);
    Files.createDirectories(elp.getParent());
    Files.writeString(elp, "installed");
    assertEquals(elp.toString(), effectivePath());

    System.setProperty(ElpConfiguration.PATH_PROPERTY, "/sandbox/elp");
    assertEquals("/sandbox/elp", effectivePath());
    System.setProperty(ElpConfiguration.PATH_PROPERTY, "");
    assertEquals("", effectivePath());
    assertFalse(ElpSettings.getInstance().isElpPositionWithServer());
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);

    store(ElpServerSource.CUSTOM, "/opt/elp", ElpMode.ELP);
    assertEquals("/opt/elp", effectivePath());
    store(ElpServerSource.CUSTOM, "", ElpMode.ELP);
    assertEquals("", effectivePath());

    store(ElpServerSource.DOWNLOADED, "/opt/elp", ElpMode.ELP);
    assertEquals(elp.toString(), effectivePath());
    Files.delete(elp);
    assertEquals("not installed: no server", "", effectivePath());
  }

  public void testResolvePath() {
    AtomicBoolean asked = new AtomicBoolean();
    Path installed = Path.of("/system/erlang-elp/elp");
    assertEquals("/p", ElpSettings.resolvePath(" /p ", ElpServerSource.DOWNLOADED, "/c", () -> {
      asked.set(true);
      return installed;
    }));
    assertEquals("", ElpSettings.resolvePath("", ElpServerSource.DOWNLOADED, "/c", () -> installed));
    assertEquals("/c", ElpSettings.resolvePath(null, ElpServerSource.CUSTOM, " /c ", () -> {
      asked.set(true);
      return installed;
    }));
    assertFalse("the download is not looked up when the property or the custom path wins", asked.get());
    assertEquals(installed.toString(), ElpSettings.resolvePath(null, ElpServerSource.DOWNLOADED, "/c", () -> installed));
    assertEquals("", ElpSettings.resolvePath(null, ElpServerSource.DOWNLOADED, "/c", () -> null));
  }

  public void testCustomPathIsConfigured() throws Exception {
    store(ElpServerSource.CUSTOM, "/opt/elp", ElpMode.PLUGIN);
    ElpAvailability availability = background(() -> ElpDistribution.getInstance().availability(getProject()));
    assertEquals(ElpAvailability.Kind.CONFIGURED, availability.kind());
    assertEquals("/opt/elp", availability.serverPath());
    assertEquals("the build is still named for the settings", myAsset, availability.choice().asset());

    System.setProperty(ElpConfiguration.PATH_PROPERTY, "");
    availability = background(() -> ElpDistribution.getInstance().availability(getProject()));
    assertEquals(ElpAvailability.Kind.CONFIGURED, availability.kind());
    assertNull(availability.serverPath());
  }

  public void testNoSdkIsUnavailable() throws Exception {
    ApplicationManager.getApplication().runWriteAction(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
    ElpAvailability availability = background(() -> ElpDistribution.getInstance().availability(getProject()));
    assertEquals(ElpAvailability.Kind.UNAVAILABLE, availability.kind());
    assertEquals(ElpAssetChoice.Reason.NO_SDK, availability.choice().reason());
    assertEquals("The project has no Erlang SDK.", availability.choice().message());
  }

  public void testElpPositionWithServer() {
    ElpSettings settings = ElpSettings.getInstance();
    assertTrue(settings.isElpPositionWithServer());
    store(ElpServerSource.DOWNLOADED, "", ElpMode.PLUGIN);
    assertFalse(settings.isElpPositionWithServer());
    store(ElpServerSource.CUSTOM, "", ElpMode.ELP);
    assertFalse(settings.isElpPositionWithServer());
    store(ElpServerSource.CUSTOM, "/opt/elp", ElpMode.ELP);
    assertTrue(settings.isElpPositionWithServer());
  }

  /** Settings stored before the source existed: a non-empty path is a custom path. */
  public void testStoredSettingsWithoutSource() {
    ElpSettings settings = ElpSettings.getInstance();
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = "/opt/elp/bin/elp";
    settings.loadState(state);
    assertEquals(ElpServerSource.CUSTOM, settings.getStoredSource());
    assertEquals("/opt/elp/bin/elp", settings.getEffectiveConfiguration().path());

    state = new ElpSettings.ElpSettingsState();
    settings.loadState(state);
    assertEquals(ElpServerSource.DOWNLOADED, settings.getStoredSource());

    state = new ElpSettings.ElpSettingsState();
    state.path = "/opt/elp/bin/elp";
    state.source = "DOWNLOADED";
    settings.loadState(state);
    assertEquals(ElpServerSource.DOWNLOADED, settings.getStoredSource());
    assertEquals("", settings.getEffectiveConfiguration().path());
  }

  private String effectivePath() throws Exception {
    return background(() -> ElpSettings.getInstance().getEffectiveConfiguration(getProject()).path());
  }

  private static void store(ElpServerSource source, String path, ElpMode mode) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.source = source.name();
    state.path = path;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
  }

  /** Runs on a background thread, as the code that resolves the server must, while the EDT keeps dispatching. */
  private static <T> T background(Callable<T> callable) throws Exception {
    FutureTask<T> task = new FutureTask<>(callable);
    new Thread(task, "ELP distribution test").start();
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(30);
    while (!task.isDone()) {
      if (System.nanoTime() > deadline) fail("timed out");
      PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
      Thread.sleep(5);
    }
    return task.get();
  }

  private static final class Outcome implements ElpDistribution.DownloadCallback {
    final List<String> events = new ArrayList<>();

    @Override
    public void installed(@NotNull Path elp) {
      events.add("installed " + elp);
    }

    @Override
    public void failed(@NotNull String message) {
      events.add("failed " + message);
    }

    @Override
    public void cancelled() {
      events.add("cancelled");
    }

    void await() throws InterruptedException {
      long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(60);
      while (events.isEmpty()) {
        if (System.nanoTime() > deadline) fail("the download did not end");
        PlatformTestUtil.dispatchAllInvocationEventsInIdeEventQueue();
        Thread.sleep(10);
      }
    }
  }
}
