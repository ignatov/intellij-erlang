package org.intellij.erlang.performance;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.ThrowableRunnable;
import org.intellij.erlang.highlighting.ErlangHighlightingTest;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

/**
 * @author ignatov
 */
public class ErlangPerformanceTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/performance/";
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    ErlangHighlightingTest.setUpInspections(myFixture);
    setUpProjectSdk();
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  protected void doTest(int expectedMs) {
    PlatformTestUtil.startPerformanceTest("erlang highlighting is slow", expectedMs, new ThrowableRunnable() {
      @Override
      public void run() throws Throwable {
        myFixture.configureByFile(getTestName(false) + ".erl");
        myFixture.doHighlighting();
      }
    }).cpuBound().usesAllCPUCores().assertTiming();
  }

  public void testDialyzerDataflow() throws Exception { doTest(5000); }
}
