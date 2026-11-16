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

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.UsefulTestCase;

import java.io.File;
import java.io.IOException;
import java.util.List;

// The roots are temporary directories of the test, which the test framework deletes in tearDown.
public class ErlangSdkTypeWindowsHomeTest extends UsefulTestCase {
  public void testOtp25InstallerDirectory() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    File home = createErlangHome(programFiles, "Erlang OTP");
    assertEquals(home.getAbsolutePath(), findHome(programFiles));
  }

  public void testOtp23InstallerDirectory() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    File home = createErlangHome(programFiles, "erl-24.3");
    assertEquals(home.getAbsolutePath(), findHome(programFiles));
  }

  public void testOldInstallerDirectory() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    File home = createErlangHome(programFiles, "erl10.7");
    assertEquals(home.getAbsolutePath(), findHome(programFiles));
  }

  public void testCurrentInstallerDirectoryFirst() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    createErlangHome(programFiles, "erl-23.0");
    File home = createErlangHome(programFiles, "Erlang OTP");
    createErlangHome(programFiles, "erl-24.3");
    assertEquals(home.getAbsolutePath(), findHome(programFiles));
  }

  public void testScoopCurrentAsRoot() throws IOException {
    File apps = FileUtil.createTempDirectory("scoop", null);
    File current = createErlangHome(apps, "current");
    assertEquals(current.getAbsolutePath(), findHome(current));
  }

  public void testLaterRoot() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    File programFilesX86 = FileUtil.createTempDirectory("ProgramFilesX86", null);
    File home = createErlangHome(programFilesX86, "Erlang OTP");
    assertEquals(home.getAbsolutePath(), findHome(programFiles, programFilesX86));
  }

  public void testDirectoryWithoutErlc() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    File home = new File(programFiles, "Erlang OTP");
    assertTrue(FileUtil.createIfDoesntExist(new File(home, "bin/erl.exe")));
    assertNull(findHome(programFiles));
  }

  public void testOtherDirectoriesAndMissingRoot() throws IOException {
    File programFiles = FileUtil.createTempDirectory("ProgramFiles", null);
    createErlangHome(programFiles, "Elixir");
    assertNull(findHome(programFiles, new File(programFiles, "missing")));
  }

  private static String findHome(File... roots) {
    return ErlangSdkType.findWindowsErlangHome(List.of(roots).stream().map(File::getAbsolutePath).toList());
  }

  private static File createErlangHome(File parent, String name) {
    File home = new File(parent, name);
    assertTrue(FileUtil.createIfDoesntExist(new File(home, "bin/erl.exe")));
    assertTrue(FileUtil.createIfDoesntExist(new File(home, "bin/erlc.exe")));
    return home;
  }
}
