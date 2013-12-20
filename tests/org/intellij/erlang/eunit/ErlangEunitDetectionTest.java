package org.intellij.erlang.eunit;

import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangEunitDetectionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/eunit/detection/";
  }

  public void testDirectInclusion() throws Exception {
    myFixture.configureByFile("direct-inclusion.erl");
    doEunitDetectionTest();
  }

  public void testIndirectInclusion() throws Exception {
    myFixture.configureByFiles("indirect-inclusion.erl", "include-eunit.hrl");
    doEunitDetectionTest();
  }

  private void doEunitDetectionTest() throws Exception {
    assertTrue(ErlangPsiImplUtil.isEunitImported((ErlangFile) myFixture.getFile()));
  }
}
