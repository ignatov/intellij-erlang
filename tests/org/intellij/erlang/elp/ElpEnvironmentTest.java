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

public class ElpEnvironmentTest extends TestCase {
  private static final String SEP = File.pathSeparator;
  private static final String INHERITED = "/usr/bin" + SEP + "/bin";

  public void testInheritedPathIsKeptWhenNothingIsConfigured() {
    assertEquals(INHERITED, ElpEnvironment.buildPath(INHERITED, null, null));
    assertEquals(INHERITED, ElpEnvironment.buildPath(INHERITED, "", ""));
    assertNull(ElpEnvironment.buildPath(null, null, null));
  }

  public void testSdkBinGoesFirst() {
    assertEquals(path("/erlang/29/bin") + SEP + INHERITED, ElpEnvironment.buildPath(INHERITED, "/erlang/29", null));
  }

  public void testRebarDirectoryGoesFirst() {
    assertEquals(path("/tools") + SEP + INHERITED, ElpEnvironment.buildPath(INHERITED, null, "/tools/rebar3"));
  }

  public void testSdkBinThenRebarDirectory() {
    assertEquals(path("/erlang/29/bin") + SEP + path("/tools") + SEP + INHERITED,
                 ElpEnvironment.buildPath(INHERITED, "/erlang/29", "/tools/rebar3"));
  }

  public void testSameDirectoryIsAddedOnce() {
    assertEquals(path("/erlang/29/bin") + SEP + INHERITED,
                 ElpEnvironment.buildPath(INHERITED, "/erlang/29", "/erlang/29/bin/rebar3"));
  }

  public void testRebarWithoutDirectoryIsSkipped() {
    assertEquals(INHERITED, ElpEnvironment.buildPath(INHERITED, null, "rebar3"));
  }

  public void testNoInheritedPath() {
    assertEquals(path("/erlang/29/bin"), ElpEnvironment.buildPath(null, "/erlang/29", null));
    assertEquals(path("/erlang/29/bin"), ElpEnvironment.buildPath("", "/erlang/29", null));
  }

  private static String path(String unixPath) {
    return new File(unixPath).getPath();
  }
}
