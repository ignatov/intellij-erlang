/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.highlighting;

public class ErlangSmallIdeHighlightingTest extends ErlangHighlightingTestBase {
  public ErlangSmallIdeHighlightingTest() {
    super(true);
  }

  public void testIncludeFromOtpIncludeDirResolve() {
    enableUnresolvedMacroInspection();
    doTest("smallIdeOtpInclude/src/IncludeResolve.erl",
      "smallIdeOtpInclude/include/testapp.hrl",
      "smallIdeOtpInclude/src/test.app.src");
  }

  public void testIncludeFromRebarConfigIncludeDirResolve() {
    enableUnresolvedMacroInspection();
    doTest("smallIdeRebarConfigInclude/src/IncludeResolve.erl",
      "smallIdeRebarConfigInclude/include/custom/testapp.hrl",
      "smallIdeRebarConfigInclude/src/test.app.src",
      "smallIdeRebarConfigInclude/rebar.config");
  }

  public void testErlang17SyntaxError() {
    enableErlang17SyntaxInspection();
    doTest();
  }

  public void testErlang18SyntaxError() {
    enableErlang18SyntaxInspection();
    doTest();
  }

  // #1727: every clause binds its own X; highlighting must not take seconds per clause (1000 clauses hung for minutes)
  public void testLongReceiveClauses() {
    long start = System.nanoTime();
    doTest();
    long ms = (System.nanoTime() - start) / 1_000_000;
    assertTrue("highlighting 200 receive clauses took " + ms + " ms", ms < 5_000);
  }
}
