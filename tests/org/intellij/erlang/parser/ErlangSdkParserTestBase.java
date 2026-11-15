package org.intellij.erlang.parser;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

/**
 * Parser tests for syntax that depends on the project SDK, which {@link ErlangParserTestBase} does not set up.
 * Compares the PSI dump with testData/parser/X.txt, like ErlangParserTest.
 */
public abstract class ErlangSdkParserTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final String mySdkHome;
  private final ErlangSdkRelease myRelease;

  protected ErlangSdkParserTestBase(@NotNull String sdkHome, @NotNull ErlangSdkRelease release) {
    mySdkHome = sdkHome;
    myRelease = release;
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk(mySdkHome, myRelease);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/parser/";
  }

  protected void doTest(@NotNull String name, boolean suppressErrors) {
    myFixture.configureByFile(name + ".erl");
    String tree = DebugUtil.psiToString(myFixture.getFile(), false, false).trim();
    assertSameLinesWithFile(getTestDataPath() + name + ".txt", tree);
    if (!suppressErrors) {
      assertFalse("PsiFile contains error elements", tree.contains("PsiErrorElement"));
    }
  }
}
