package org.intellij.erlang.highlighting.generate;

import com.intellij.testFramework.PlatformTestUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangGenerateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private void doTest(String actionId, String before, String after) {
    myFixture.configureByText("a.erl", before);
    PlatformTestUtil.invokeNamedAction(actionId);
    myFixture.checkResult(after);
  }

  public void testEunitFail() throws Exception {
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

  public void testEunitFunctionWithoutName() throws Exception {
    doTest("EUnitGenerateTestMethod",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "<caret>",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n" +
        "name_test() ->\n" +
        "  ?assertEqual(expected, expr).");

  }

  public void testEunitFunctionNewLines() throws Exception {
    doTest("EUnitGenerateTestMethod",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n<caret>",
      "-include_lib(\"eunit/include/eunit.hrl\").\n" +
        "\n" +
        "\n" +
        "name_test() ->\n" +
        "  ?assertEqual(expected, expr).");

  }

  public void testEunitWithName() throws Exception {
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
