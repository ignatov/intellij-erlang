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
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.io.File;
import java.io.IOException;
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
