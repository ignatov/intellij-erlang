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

import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.NioFiles;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.zip.GZIPOutputStream;

/**
 * The download and the installation of an ELP tarball, from a local HTTP server, with a small fake tarball: nothing goes to the
 * network. After a failure nothing is left in the installation directory.
 */
public class ElpInstallerTest extends BasePlatformTestCase {
  /** A shell script that answers {@code elp version}; the random tail makes the download take several reads. */
  static final byte[] FAKE_ELP = fakeElp("1.1.0+build-test");

  private HttpServer myServer;
  private final AtomicInteger myRequests = new AtomicInteger();
  private volatile Behaviour myBehaviour = Behaviour.SERVE;
  private byte[] myTarball;
  private Path myBaseDir;
  private final List<Path> myVersionChecks = new ArrayList<>();

  enum Behaviour {SERVE, NOT_FOUND, DROP_HALFWAY}

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myTarball = tarGz("elp", FAKE_ELP);
    myBaseDir = Files.createTempDirectory("erlang-elp-test");
    myServer = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    myServer.createContext("/dl/", this::handle);
    myServer.start();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
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

  private void handle(HttpExchange exchange) throws IOException {
    myRequests.incrementAndGet();
    try (exchange) {
      switch (myBehaviour) {
        case NOT_FOUND -> exchange.sendResponseHeaders(404, -1);
        case SERVE -> {
          exchange.sendResponseHeaders(200, myTarball.length);
          try (OutputStream out = exchange.getResponseBody()) {
            out.write(myTarball);
          }
        }
        case DROP_HALFWAY -> {
          exchange.sendResponseHeaders(200, myTarball.length);
          OutputStream out = exchange.getResponseBody();
          out.write(myTarball, 0, myTarball.length / 2);
          out.flush();
          // closing the exchange before the announced length was sent drops the connection
        }
      }
    }
  }

  private String downloadUrl() {
    return "http://127.0.0.1:" + myServer.getAddress().getPort() + "/dl/";
  }

  private ElpRelease.Asset asset(long size, String sha256) {
    return new ElpRelease.Asset("elp-macos-aarch64-apple-darwin-otp-28.5.tar.gz", "macos", "aarch64", "aarch64-apple-darwin", "28.5",
                                size, sha256, "elp");
  }

  private ElpRelease.Asset goodAsset() {
    return asset(myTarball.length, sha256(myTarball));
  }

  private ElpInstaller installer(ElpRelease.Asset asset) {
    return installer(asset, (executable, release, indicator) -> myVersionChecks.add(executable));
  }

  private ElpInstaller installer(ElpRelease.Asset asset, ElpInstaller.VersionCheck check) {
    ElpRelease release = new ElpRelease("test-tag", "1.1.0", "https://example.invalid/repo", "https://example.invalid/dl/",
                                        "MIT OR Apache-2.0", List.of(asset));
    return new ElpInstaller(myBaseDir, release, downloadUrl(), check);
  }

  public void testSuccess() throws Exception {
    ElpRelease.Asset asset = goodAsset();
    ElpInstaller installer = installer(asset);
    assertNull(installer.findInstalled(asset));
    List<String> texts = new ArrayList<>();
    Path elp = install(installer, asset, new EmptyProgressIndicator() {
      @Override
      public void setText(String text) {
        texts.add(text);
      }
    });
    assertEquals(myBaseDir.resolve("test-tag").resolve("elp-macos-aarch64-apple-darwin-otp-28.5").resolve("elp"), elp);
    assertEquals(elp, installer.findInstalled(asset));
    assertTrue(Files.isRegularFile(elp));
    if (!SystemInfo.isWindows) assertTrue(Files.isExecutable(elp));
    assertTrue(Arrays.equals(FAKE_ELP, Files.readAllBytes(elp)));
    assertEquals(List.of("elp"), childNames(elp.getParent()));
    assertEquals("the unpacked binary is checked before it is moved", 1, myVersionChecks.size());
    assertFalse(myVersionChecks.get(0).equals(elp));
    assertEquals(String.format("Downloading ELP 1.1.0, %.1f MB", myTarball.length / 1_000_000.0), texts.get(0));
    assertEquals(1, myRequests.get());
  }

  public void testWrongChecksum() {
    String wrong = "0".repeat(64);
    ElpRelease.Asset asset = asset(myTarball.length, wrong);
    String message = installFails(installer(asset), asset);
    assertTrue(message, message.contains("wrong checksum"));
    assertTrue(message, message.contains("expected sha256 " + wrong));
    assertTrue(message, message.contains("got " + sha256(myTarball)));
    assertNothingInstalled(asset);
    assertEmpty("nothing is unpacked or run after a wrong checksum", myVersionChecks);
  }

  public void testWrongSize() {
    ElpRelease.Asset smaller = asset(myTarball.length + 10, sha256(myTarball));
    String message = installFails(installer(smaller), smaller);
    assertTrue(message, message.contains("wrong size: expected " + (myTarball.length + 10) + " bytes, got " + myTarball.length + "."));
    assertNothingInstalled(smaller);

    ElpRelease.Asset larger = asset(myTarball.length - 10, sha256(myTarball));
    message = installFails(installer(larger), larger);
    assertTrue(message, message.contains("wrong size: expected " + (myTarball.length - 10) + " bytes, got more."));
    assertNothingInstalled(larger);
    assertEmpty(myVersionChecks);
  }

  public void testHttpNotFound() {
    myBehaviour = Behaviour.NOT_FOUND;
    ElpRelease.Asset asset = goodAsset();
    String message = installFails(installer(asset), asset);
    assertEquals("Cannot download ELP: HTTP 404 from " + downloadUrl() + asset.file() + ".", message);
    assertNothingInstalled(asset);
  }

  public void testConnectionDroppedHalfway() {
    myBehaviour = Behaviour.DROP_HALFWAY;
    ElpRelease.Asset asset = goodAsset();
    String message = installFails(installer(asset), asset);
    assertTrue(message, message.startsWith("Cannot download ELP from " + downloadUrl() + asset.file() + ": ") ||
                        message.contains("wrong size"));
    assertNothingInstalled(asset);
  }

  public void testNoServer() throws Exception {
    ElpRelease.Asset asset = goodAsset();
    ElpInstaller installer = installer(asset);
    myServer.stop(0);
    String message = installFails(installer, asset);
    assertTrue(message, message.startsWith("Cannot download ELP from "));
    assertNothingInstalled(asset);
  }

  public void testCancel() {
    ElpRelease.Asset asset = goodAsset();
    ProgressIndicator indicator = new EmptyProgressIndicator() {
      @Override
      public void setFraction(double fraction) {
        super.setFraction(fraction);
        cancel();
      }
    };
    try {
      install(installer(asset), asset, indicator);
      fail("not cancelled");
    }
    catch (ProcessCanceledException expected) {
      // cancelled
    }
    catch (ElpInstallException e) {
      fail(e.getMessage());
    }
    assertNothingInstalled(asset);
  }

  public void testExistingInstallIsReused() throws Exception {
    ElpRelease.Asset asset = goodAsset();
    ElpInstaller installer = installer(asset);
    Path elp = installer.executable(asset);
    Files.createDirectories(elp.getParent());
    Files.writeString(elp, "installed before");
    assertEquals(elp, install(installer, asset, new EmptyProgressIndicator()));
    assertEquals(0, myRequests.get());
    assertEquals("installed before", Files.readString(elp));
  }

  public void testInterruptedDownloadIsCleanedUp() throws Exception {
    ElpRelease.Asset asset = goodAsset();
    ElpInstaller installer = installer(asset);
    Path directory = installer.installDirectory(asset);
    Files.createDirectories(directory.resolve(".unpack-123"));
    Files.writeString(directory.resolve(".unpack-123").resolve("elp"), "half");
    Files.writeString(directory.resolve(".download-456.tar.gz"), "half");
    Path elp = install(installer, asset, new EmptyProgressIndicator());
    assertEquals(List.of("elp"), childNames(directory));
    assertTrue(Arrays.equals(FAKE_ELP, Files.readAllBytes(elp)));
  }

  public void testTarballWithoutTheExecutable() throws Exception {
    myTarball = tarGz("README", "no binary".getBytes(StandardCharsets.UTF_8));
    ElpRelease.Asset asset = goodAsset();
    String message = installFails(installer(asset), asset);
    assertEquals(asset.file() + " has no elp.", message);
    assertNothingInstalled(asset);
  }

  /** A binary that does not run is reported and removed, not kept. */
  public void testFailedVersionCheckIsNotKept() {
    ElpRelease.Asset asset = goodAsset();
    String message = installFails(installer(asset, (executable, release, indicator) -> {
      throw new ElpInstallException("The downloaded ELP does not run: 'elp version' exited with 1.");
    }), asset);
    assertEquals("The downloaded ELP does not run: 'elp version' exited with 1.", message);
    assertNothingInstalled(asset);
  }

  /** The real check runs the unpacked file: the fake one is a shell script, so not on Windows. */
  public void testRealVersionCheck() throws Exception {
    if (SystemInfo.isWindows) return;
    ElpRelease.Asset asset = goodAsset();
    Path elp = install(installer(asset, ElpInstaller::runVersion), asset, new EmptyProgressIndicator());
    assertEquals(elp, installer(asset).findInstalled(asset));

    myTarball = tarGz("elp", fakeElp("0.9.0"));
    ElpRelease.Asset other = asset(myTarball.length, sha256(myTarball));
    NioFiles.deleteRecursively(myBaseDir.resolve("test-tag"));
    String message = installFails(installer(other, ElpInstaller::runVersion), other);
    assertEquals("The downloaded ELP printed 'elp 0.9.0' for 'elp version', expected version 1.1.0.", message);
    assertNothingInstalled(other);

    myTarball = tarGz("elp", "\u007fELF not a binary for this machine".getBytes(StandardCharsets.UTF_8));
    ElpRelease.Asset broken = asset(myTarball.length, sha256(myTarball));
    message = installFails(installer(broken, ElpInstaller::runVersion), broken);
    assertTrue(message, message.startsWith("The downloaded ELP does not start: ") || message.startsWith("The downloaded ELP does not run: "));
    assertNothingInstalled(broken);
  }

  private void assertNothingInstalled(ElpRelease.Asset asset) {
    ElpInstaller installer = installer(asset);
    assertNull(installer.findInstalled(asset));
    Path directory = installer.installDirectory(asset);
    if (Files.isDirectory(directory)) assertEmpty(childNames(directory));
  }

  private static List<String> childNames(Path directory) {
    return NioFiles.list(directory).stream().map(p -> p.getFileName().toString()).sorted().toList();
  }

  private static String installFails(ElpInstaller installer, ElpRelease.Asset asset) {
    try {
      install(installer, asset, new EmptyProgressIndicator());
      fail("installed");
      return null;
    }
    catch (ElpInstallException e) {
      return e.getMessage();
    }
  }

  /**
   * HttpRequests refuses to run on the EDT, where light tests run. A plain thread, because the application pool logs the exceptions
   * of its tasks as errors and swallows cancellation.
   */
  static Path install(ElpInstaller installer, ElpRelease.Asset asset, ProgressIndicator indicator) throws ElpInstallException {
    FutureTask<Path> task = new FutureTask<>(() -> installer.install(asset, indicator));
    new Thread(task, "ELP install test").start();
    try {
      return task.get(60, TimeUnit.SECONDS);
    }
    catch (ExecutionException e) {
      if (e.getCause() instanceof ElpInstallException installException) throw installException;
      if (e.getCause() instanceof ProcessCanceledException cancelled) throw cancelled;
      throw new AssertionError(e.getCause());
    }
    catch (Exception e) {
      throw new AssertionError(e);
    }
  }

  @NotNull
  static byte[] fakeElp(String version) {
    byte[] script = ("#!/bin/sh\necho 'elp " + version + "'\nexit 0\n").getBytes(StandardCharsets.UTF_8);
    byte[] tail = new byte[300_000];
    new Random(42).nextBytes(tail);
    byte[] result = new byte[script.length + tail.length];
    System.arraycopy(script, 0, result, 0, script.length);
    System.arraycopy(tail, 0, result, script.length, tail.length);
    return result;
  }

  @NotNull
  static byte[] tarGz(String name, byte[] content) throws IOException {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (TarArchiveOutputStream tar = new TarArchiveOutputStream(new GZIPOutputStream(bytes))) {
      TarArchiveEntry entry = new TarArchiveEntry(name);
      entry.setSize(content.length);
      entry.setMode(0100644);
      tar.putArchiveEntry(entry);
      tar.write(content);
      tar.closeArchiveEntry();
    }
    return bytes.toByteArray();
  }

  @NotNull
  static String sha256(byte[] bytes) {
    try {
      return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
    }
    catch (Exception e) {
      throw new AssertionError(e);
    }
  }
}
