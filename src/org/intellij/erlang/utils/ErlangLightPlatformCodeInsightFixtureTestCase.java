package org.intellij.erlang.utils;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;

/**
 * @author ignatov
 */
public abstract class ErlangLightPlatformCodeInsightFixtureTestCase extends LightPlatformCodeInsightFixtureTestCase {
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
