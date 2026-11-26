package org.intellij.erlang.parser;

import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

public class ErlangNoSdkParserTest extends ErlangSdkParserTestBase {
  public ErlangNoSdkParserTest() {
    super(null, null);
  }

  public void testSigilControlEscapeBadName() {
    assertEquals(ErlangSdkRelease.DefaultRelease, ErlangSdkType.getRelease(getProject()));
    doTest("SigilControlEscapeBadName", true);
  }
}
