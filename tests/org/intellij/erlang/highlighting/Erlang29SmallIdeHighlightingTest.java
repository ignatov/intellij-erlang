package org.intellij.erlang.highlighting;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkForSmallIdes;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

public class Erlang29SmallIdeHighlightingTest extends ErlangHighlightingTestBase {
  private static final String SDK_HOME = "testData/mockSdk-R29";

  public Erlang29SmallIdeHighlightingTest() {
    super(true);
  }

  @Nullable
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    // we create an sdk here so that we have sdk version cached
    ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_29_0);
    ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), SDK_HOME);
  }

  @Override
  protected void tearDown() throws Exception {
    ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), "");
    super.tearDown();
  }

  public void testCorrectSdkReleaseIsReported() {
    assertEquals(ErlangSdkRelease.V_29_0.getOtpRelease(), Objects.requireNonNull(ErlangSdkType.getRelease(getProject())).getOtpRelease());
  }

  public void testErlang29SyntaxNoError() {
    enableErlang29SyntaxInspection();
    doTest();
  }
}
