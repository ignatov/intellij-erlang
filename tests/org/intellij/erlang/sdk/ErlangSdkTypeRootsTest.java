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

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ui.configuration.projectRoot.ProjectSdksModel;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jdom.Element;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

// The homes are temporary directories of the test, which the test framework deletes in tearDown.
public class ErlangSdkTypeRootsTest extends BasePlatformTestCase {
  // An installation prefix such as /usr, /usr/local or /opt/homebrew: bin/erl, lib/erlang/lib/<app>, and other libraries in lib.
  public void testPrefixHomeUsesErlangLibNotWholeLib() throws IOException {
    File home = FileUtil.createTempDirectory("prefix", null);
    createFile(home, "lib/junk/x.txt");
    createFile(home, "lib/erlang/lib/stdlib-1/src/a.erl");
    assertEquals(List.of(path(home, "lib/erlang/lib")), roots(home, OrderRootType.CLASSES));
    assertEquals(List.of(path(home, "lib/erlang/lib")), roots(home, OrderRootType.SOURCES));
  }

  // An OTP root such as /usr/lib/erlang or /usr/local/lib/erlang: lib holds only the OTP applications.
  public void testOtpRootHomeUsesLib() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    createFile(home, "lib/stdlib-1/src/a.erl");
    assertEquals(List.of(path(home, "lib")), roots(home, OrderRootType.CLASSES));
    assertEquals(List.of(path(home, "lib")), roots(home, OrderRootType.SOURCES));
  }

  // OTP 17 on: releases/<release>/OTP_VERSION holds the full OTP version, and erts-<version> is in the home
  public void testVersionFromOtpVersionFileAndErtsDirectory() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    writeFile(home, "releases/27/OTP_VERSION", "27.1.2\n");
    assertTrue(new File(home, "erts-15.1.2").mkdirs());
    assertEquals("OTP 27.1.2, erts-15.1.2", ErlangSdkType.getInstance().getVersionString(home.getAbsolutePath()));
    assertEquals("Erlang 27.1.2", ErlangSdkType.getInstance().suggestSdkName(null, home.getAbsolutePath()));
  }

  // an installation prefix keeps erts-<version> in lib
  public void testVersionFromOtpVersionFileAndErtsDirectoryInLib() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    writeFile(home, "releases/26/OTP_VERSION", "26.2.5");
    assertTrue(new File(home, "lib/erts-14.2.5").mkdirs());
    assertEquals("OTP 26.2.5, erts-14.2.5", ErlangSdkType.getInstance().getVersionString(home.getAbsolutePath()));
  }

  // without an erts directory the OTP_VERSION file is not enough, and releases/start_erl.data ("<erts> <release>") is read
  public void testVersionFromStartErlData() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    writeFile(home, "releases/27/OTP_VERSION", "27.1.2");
    writeFile(home, "releases/start_erl.data", "15.1.2 27\n");
    assertEquals("OTP 27, erts-15.1.2", ErlangSdkType.getInstance().getVersionString(home.getAbsolutePath()));
  }

  public void testVersionFromStartErlDataOfPrefix() throws IOException {
    File home = FileUtil.createTempDirectory("prefix", null);
    writeFile(home, "lib/erlang/releases/start_erl.data", "5.10.4 R16B03");
    assertEquals("OTP R16B03, erts-5.10.4", ErlangSdkType.getInstance().getVersionString(home.getAbsolutePath()));
    assertEquals("Erlang R16B03", ErlangSdkType.getInstance().suggestSdkName("Erlang", home.getAbsolutePath()));
  }

  // a start_erl.data without both versions is not used, and without erl the version cannot be detected
  public void testMalformedStartErlDataWithoutErl() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    writeFile(home, "releases/start_erl.data", "15.1.2");
    assertVersionNotDetected(home);
  }

  public void testEmptyHomeWithoutErl() throws IOException {
    assertVersionNotDetected(FileUtil.createTempDirectory("empty", null));
  }

  public void testHomeWithEmptyErl() throws IOException {
    File home = FileUtil.createTempDirectory("empty", null);
    assertTrue(FileUtil.createIfDoesntExist(JpsErlangSdkType.getByteCodeInterpreterExecutable(home.getAbsolutePath())));
    assertVersionNotDetected(home);
  }

  public void testValidSdkHomeHasErlAndErlc() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    createExecutable(JpsErlangSdkType.getByteCodeInterpreterExecutable(home.getAbsolutePath()));
    assertFalse(ErlangSdkType.getInstance().isValidSdkHome(home.getAbsolutePath()));
    createExecutable(JpsErlangSdkType.getByteCodeCompilerExecutable(home.getAbsolutePath()));
    assertTrue(ErlangSdkType.getInstance().isValidSdkHome(home.getAbsolutePath()));
  }

  public void testMissingHomeIsNotValid() throws IOException {
    File home = new File(FileUtil.createTempDirectory("otp", null), "missing");
    assertFalse(ErlangSdkType.getInstance().isValidSdkHome(home.getAbsolutePath()));
  }

  public void testDocumentationUrl() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    createFile(home, "lib/stdlib-1/src/a.erl");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(home));
    Sdk sdk25 = ErlangSdkType.createMockSdk(home.getAbsolutePath(), ErlangSdkRelease.V_25_0);
    assertEquals("https://www.erlang.org/doc", ErlangSdkType.getInstance().getDefaultDocumentationUrl(sdk25));
    Sdk sdk27 = ErlangSdkType.createMockSdk(home.getAbsolutePath(), new ErlangSdkRelease("27.1", "15.1"));
    assertEquals("https://www.erlang.org/docs/27", ErlangSdkType.getInstance().getDefaultDocumentationUrl(sdk27));
  }

  public void testNoAdditionalData() throws IOException {
    File home = FileUtil.createTempDirectory("otp", null);
    createFile(home, "lib/stdlib-1/src/a.erl");
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(home));
    Sdk sdk = ErlangSdkType.createMockSdk(home.getAbsolutePath(), ErlangSdkRelease.V_27_0);
    assertNull(ErlangSdkType.getInstance().createAdditionalDataConfigurable(new ProjectSdksModel(), sdk.getSdkModificator()));
    Element additional = new Element("additional");
    ErlangSdkType.getInstance().saveAdditionalData(new SdkAdditionalData() {}, additional);
    assertTrue(additional.getContent().isEmpty());
    assertTrue(additional.getAttributes().isEmpty());
  }

  // asdf keeps each release in ~/.asdf/installs/erlang/<version> with OTP in lib/erlang, Scoop in ~\scoop\apps\erlang\current
  public void testSuggestHomePathUnderUserHome() throws IOException {
    File userHome = FileUtil.createTempDirectory("home", null);
    createFile(userHome, ".asdf/installs/erlang/27.0/bin/erl");
    File asdfHome = new File(userHome, ".asdf/installs/erlang/27.0/lib/erlang");
    assertTrue(asdfHome.mkdirs());
    File scoopHome = new File(userHome, "scoop/apps/erlang/current");
    createFile(scoopHome, "bin/erl.exe");
    createFile(scoopHome, "bin/erlc.exe");
    String expected = (SystemInfo.isWindows ? scoopHome : asdfHome).getAbsolutePath();
    String oldUserHome = System.getProperty("user.home");
    System.setProperty("user.home", userHome.getAbsolutePath());
    try {
      assertEquals(expected, ErlangSdkType.getInstance().suggestHomePath());
    }
    finally {
      System.setProperty("user.home", oldUserHome);
    }
  }

  private static void assertVersionNotDetected(File home) {
    try {
      ErlangSdkType.getInstance().getVersionString(home.getAbsolutePath());
      fail("the version of " + home + " was detected");
    }
    catch (AssertionError e) {
      assertTrue(e.getMessage(), e.getMessage().startsWith("SDK version detection failed."));
    }
  }

  private static void writeFile(File home, String relativePath, String text) throws IOException {
    FileUtil.writeToFile(new File(home, relativePath), text.getBytes(StandardCharsets.UTF_8));
  }

  private static void createExecutable(File file) {
    assertTrue(FileUtil.createIfDoesntExist(file));
    assertTrue(file.setExecutable(true));
  }

  private static List<String> roots(File home, OrderRootType type) {
    assertNotNull(LocalFileSystem.getInstance().refreshAndFindFileByIoFile(home));
    Sdk sdk = ErlangSdkType.createMockSdk(home.getAbsolutePath(), ErlangSdkRelease.V_27_0);
    return Arrays.stream(sdk.getRootProvider().getFiles(type)).map(VirtualFile::getPath).toList();
  }

  private static String path(File home, String relativePath) {
    return FileUtil.toSystemIndependentName(new File(home, relativePath).getAbsolutePath());
  }

  private static void createFile(File home, String relativePath) {
    assertTrue(FileUtil.createIfDoesntExist(new File(home, relativePath)));
  }
}
