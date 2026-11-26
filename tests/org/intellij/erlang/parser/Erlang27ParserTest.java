package org.intellij.erlang.parser;

import org.intellij.erlang.sdk.ErlangSdkRelease;

public class Erlang27ParserTest extends ErlangSdkParserTestBase {
  public Erlang27ParserTest() {
    super("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0);
  }

  public void testControlEscapeBadNameErrors() { doTest("ControlEscapeBadNameErrors", true); }
}
