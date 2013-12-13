package org.intellij.erlang.utils;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import com.intellij.util.PlatformUtils;
import com.intellij.util.PlatformUtilsCore;

/**
 * @author ignatov
 */
public abstract class ErlangLightPlatformCodeInsightFixtureTestCase extends LightPlatformCodeInsightFixtureTestCase {
  private String myPlatformPrefix;
  private String myBackedUpPlatformPrefix;

  protected ErlangLightPlatformCodeInsightFixtureTestCase() {
  }

  protected ErlangLightPlatformCodeInsightFixtureTestCase(String platformPrefix) {
    myPlatformPrefix = platformPrefix;
  }

  @Override
  protected void setUp() throws Exception {
    if (myPlatformPrefix != null) {
      myBackedUpPlatformPrefix = PlatformUtils.getPlatformPrefix();
      System.setProperty(PlatformUtilsCore.PLATFORM_PREFIX_KEY, myPlatformPrefix);
    }
    super.setUp();
  }

  @Override
  protected void tearDown() throws Exception {
    if (myPlatformPrefix != null) {
      System.setProperty(PlatformUtilsCore.PLATFORM_PREFIX_KEY, myBackedUpPlatformPrefix);
    }
    super.tearDown();
  }

  protected void setUpProjectSdk() {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        final Sdk sdk = getProjectDescriptor().getSdk();
        ProjectJdkTable.getInstance().addJdk(sdk);
        ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
      }
    });
  }
}
