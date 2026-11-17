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

import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.io.File;
import java.io.IOException;

public class ExtProcessUtilTest extends BasePlatformTestCase {
  // The rebar version check runs through execAndGetFirstLine; a rebar3 that loses its stdout
  // writes rebar3.crashdump into its working directory, so that must not be the IDE's one.
  public void testCommandRunsInTempDirectory() throws IOException {
    String[] printWorkingDirectory = SystemInfo.isWindows ? new String[]{"cmd", "/c", "cd"} : new String[]{"pwd"};
    String workingDirectory = ExtProcessUtil.execAndGetFirstLine(10000, printWorkingDirectory).getStdOut();
    assertFalse("no output from " + String.join(" ", printWorkingDirectory), workingDirectory.isEmpty());
    assertEquals(new File(FileUtil.getTempDirectory()).getCanonicalPath(), new File(workingDirectory).getCanonicalPath());
  }
}
