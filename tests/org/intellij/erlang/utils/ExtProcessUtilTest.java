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

package org.intellij.erlang.utils;

import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;

import java.io.File;

public class ExtProcessUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // Started in a child JVM: prints the directory that the child was started in.
  public static final class PrintWorkingDirectory {
    public static void main(String[] args) {
      System.out.println(System.getProperty("user.dir"));
    }
  }

  // rebar3 writes rebar3.crashdump into its working directory, which must not be the directory of the IDE or the project.
  public void testCommandRunsInTempDirectory() throws Exception {
    File java = new File(System.getProperty("java.home"), "bin/" + (SystemInfo.isWindows ? "java.exe" : "java"));
    String classPath = PathManager.getJarPathForClass(PrintWorkingDirectory.class);
    assertNotNull(classPath);

    String workDirectory = ExtProcessUtil.execAndGetFirstLine(60000, java.getPath(), "-cp", classPath,
                                                              PrintWorkingDirectory.class.getName()).getStdOut();

    assertFalse("The child JVM printed nothing", workDirectory.isEmpty());
    assertEquals(new File(FileUtil.getTempDirectory()).getCanonicalPath(), new File(workDirectory).getCanonicalPath());
  }
}
