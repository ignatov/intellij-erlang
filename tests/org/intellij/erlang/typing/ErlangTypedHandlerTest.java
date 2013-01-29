/*
 * Copyright 2013 Sergey Ignatov
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

package org.intellij.erlang.typing;

import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;

/**
 * @author ignatov
 */
public class ErlangTypedHandlerTest extends LightPlatformCodeInsightFixtureTestCase {
  public void testFunctionClause() throws Exception {
    doTest("foo(A, B, C) -> ok<caret>",
      "foo(A, B, C) -> ok;\n" +
        "foo(A, B, C) -> ");
  }

  public void testEmptyFunctionClause() throws Exception {
    doTest("foo() -> ok<caret>",
      "foo() -> ok;\n" +
        "foo() -> <caret>");
  }

  public void testCaseClause() throws Exception {
    doTest(
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2<caret>",
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2;\n" +
        "    _ -> ");
  }

//  public void testBinaryHandler() throws Exception {
//    doTest("foo() -> <<caret>", "foo() -> <<<caret>>>", "<");
//  }

  private void doTest(String before, String after) {
    doTest(before, after, ";");
  }

  private void doTest(String before, String after, String symbol) {
    myFixture.configureByText("a.erl", before);
    myFixture.type(symbol);
    myFixture.checkResult(after);
  }
}
