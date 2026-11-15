package org.intellij.erlang.parser;

import org.intellij.erlang.sdk.ErlangSdkRelease;

public class Erlang25ParserTest extends ErlangSdkParserTestBase {
  public Erlang25ParserTest() {
    super("testData/mockSdk-R25/", ErlangSdkRelease.V_25_0);
  }

  public void testControlEscapeAnyCharOtp25() { doTest("ControlEscapeAnyCharOtp25", false); }
}
