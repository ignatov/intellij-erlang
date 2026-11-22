package org.intellij.erlang.highlighting;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

public class Erlang27_0HighlightingTest extends ErlangHighlightingTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        // OTP 27.0 ships erts-15.0, as detected from the erts-* directory of a real installation
        return ErlangSdkType.createMockSdk("testData/mockSdk-R27/", new ErlangSdkRelease("27.0", "15.0"));
      }
    };
  }

  public void testErlang27_0SyntaxNoError() {
    enableErlang27SyntaxInspection();
    doTest();
  }
}
