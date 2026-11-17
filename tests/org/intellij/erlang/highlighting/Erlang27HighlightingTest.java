package org.intellij.erlang.highlighting;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

public class Erlang27HighlightingTest extends ErlangHighlightingTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0);
      }
    };
  }

  public void testErlang27SyntaxNoError() {
    enableErlang27SyntaxInspection();
    doTest();
  }

  public void testTripleQuotedStringWhiteSpaceAfterOpening() {
    doTest();
  }

  public void testTripleQuotedStringVisibleCharAfterOpening() {
    doTest();
  }

  public void testIoFormatSigils() {
    myFixture.addFileToProject("io.erl", "-module(io).\n-export([format/2]).\nformat(_, _) -> ok.\n");
    myFixture.addFileToProject("io_lib.erl", "-module(io_lib).\n-export([format/2]).\nformat(_, _) -> [].\n");
    doTest();
  }

  public void testErlang27MoreThanThreeQuotes() {
    enableErlang27SyntaxInspection();
    doTest();
  }

  public void testWhitespaceAfterTripleQuotedStringOpening() {
    myFixture.configureByText("whitespace.erl",
                              "-module(whitespace).\n" +
                              "-export([three/0, four/0, five/0, seven/0, sigil/0]).\n" +
                              "three() -> \"\"\"   \n  text\n  \"\"\".\n" +
                              "four() -> \"\"\"\"   \n  text\n  \"\"\"\".\n" +
                              "five() -> \"\"\"\"\"   \n  text\n  \"\"\"\"\".\n" +
                              "seven() -> \"\"\"\"\"\"\"   \n  text\n  \"\"\"\"\"\"\".\n" +
                              "sigil() -> ~b\"\"\"\"   \n  text\n  \"\"\"\".\n");
    setUpInspections(myFixture);
    enableErlang27SyntaxInspection();
    myFixture.checkHighlighting(true, false, false);
  }

  public void testErlang29SyntaxError() {
    enableErlang29SyntaxInspection();
    doTest();
  }

  public void testErlang29SyntaxSingleValuedComprehensions() {
    enableErlang29SyntaxInspection();
    doTest();
  }

  public void testAutoImportByRelease27() {
    addErlangModuleWithFunctionsAutoImportedSince28();
    doTest();
  }
}
