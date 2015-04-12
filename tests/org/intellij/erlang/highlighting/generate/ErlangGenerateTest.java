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

package org.intellij.erlang.highlighting.generate;

import com.intellij.testFramework.PlatformTestUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangGenerateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private void doTest(String actionId, String before, String after) {
    myFixture.configureByText("a.erl", before);
    PlatformTestUtil.invokeNamedAction(actionId);
    myFixture.checkResult(after);
  }

  public void testEunitFail() {
    Throwable ee = null;
    try {
      doTest("EUnitGenerateTestMethod",
        "",
        "");
    } catch (Throwable e) {
      ee = e;
    }
    assertNotNull(ee);

  }

  public void testEunitFunctionWithoutName() {
    doTest("EUnitGenerateTestMethod",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "<caret>",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n" +
        "name_test() ->\n" +
        "  ?assertEqual(expected, expr).");

  }

  public void testEunitFunctionNewLines() {
    doTest("EUnitGenerateTestMethod",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n<caret>",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n" +
        "\n" +
        "name_test() ->\n" +
        "  ?assertEqual(expected, expr).");

  }

  public void testEunitWithName() {
    doTest("EUnitGenerateTestMethod",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "foo() -> ok<caret>.",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "foo() -> ok.\n" +
        "\n" +
        "foo_test() ->\n" +
        "  ?assertEqual(expected, expr).");
  }
}
