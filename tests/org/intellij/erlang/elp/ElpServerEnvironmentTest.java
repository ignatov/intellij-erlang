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

package org.intellij.erlang.elp;

import junit.framework.TestCase;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class ElpServerEnvironmentTest extends TestCase {
  private static final String SEP = File.pathSeparator;
  private static final String MINIMAL = "/usr/bin" + SEP + "/bin" + SEP + "/usr/sbin" + SEP + "/sbin";

  /** What an IDE started from the Dock gives: no erl, no rebar3. The SDK's bin goes first, and rebar3 comes as ELP_REBAR3. */
  public void testDockPathGetsSdkBinFirstAndRebar3() {
    Path tmp = Path.of("/var/elp-tmp/session-1");
    Map<String, String> variables = ElpServerEnvironment.build(MINIMAL, "/erlang/29", "/tools/rebar3", tmp);
    assertEquals(path("/erlang/29/bin") + SEP + path("/tools") + SEP + MINIMAL, variables.get(ElpServerEnvironment.PATH));
    assertEquals("/tools/rebar3", variables.get(ElpServerEnvironment.ELP_REBAR3));
    assertEquals(tmp.toString(), variables.get(ElpServerEnvironment.TMPDIR));
  }

  public void testOldRebarIsNotElpRebar3() {
    Map<String, String> variables = ElpServerEnvironment.build(MINIMAL, "/erlang/29", "/tools/rebar", null);
    assertFalse(variables.containsKey(ElpServerEnvironment.ELP_REBAR3));
    assertFalse(variables.containsKey(ElpServerEnvironment.TMPDIR));
  }

  public void testNothingConfigured() {
    assertEquals(Map.of(ElpServerEnvironment.PATH, MINIMAL), ElpServerEnvironment.build(MINIMAL, null, "", null));
    assertEquals(Map.of(), ElpServerEnvironment.build(null, null, null, null));
  }

  public void testSessionDirectoriesAreNewAndEndedOnesAreDeleted() throws Exception {
    Path first = ElpServerEnvironment.createSessionDirectory();
    assertNotNull(first);
    Files.writeString(first.resolve("eqwalizerXXXXXX"), "left by ELP");
    Path leftByACrashedIde = Files.createDirectories(ElpServerEnvironment.sessionsBase().resolve("session-1-1"));
    Files.writeString(leftByACrashedIde.resolve("eqwalizerYYYYYY"), "left by ELP");

    Path second = ElpServerEnvironment.createSessionDirectory();
    assertNotNull(second);
    assertFalse(first.equals(second));
    assertTrue("a running server keeps its directory", Files.isDirectory(first));
    assertFalse("a directory of an ended session is deleted at the next start", Files.exists(leftByACrashedIde));

    ElpServerEnvironment.deleteSessionDirectory(first);
    assertFalse(Files.exists(first));
    ElpServerEnvironment.deleteSessionDirectory(second);
    assertFalse(Files.exists(second));
  }

  private static String path(String unixPath) {
    return new File(unixPath).getPath();
  }
}
