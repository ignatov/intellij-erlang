/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.utils;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import com.intellij.util.PlatformUtils;

public abstract class ErlangLightPlatformCodeInsightFixtureTestCase extends LightPlatformCodeInsightFixtureTestCase {
  private String myBackedUpPlatformPrefix;
  private final boolean myIsSmallIde;

  protected ErlangLightPlatformCodeInsightFixtureTestCase(boolean isSmallIde) {
    myIsSmallIde = isSmallIde;
  }

  protected ErlangLightPlatformCodeInsightFixtureTestCase() {
    myIsSmallIde = false;
  }

  @Override
  protected void setUp() throws Exception {
    if (myIsSmallIde) {
      myBackedUpPlatformPrefix = PlatformUtils.getPlatformPrefix();
      System.setProperty(PlatformUtils.PLATFORM_PREFIX_KEY, PlatformUtils.PYCHARM_PREFIX);
    }
    super.setUp();
  }

  @Override
  protected void tearDown() throws Exception {
    if (myIsSmallIde) {
      System.setProperty(PlatformUtils.PLATFORM_PREFIX_KEY, myBackedUpPlatformPrefix);
    }
    super.tearDown();
  }

  protected void setUpProjectSdk() {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        Sdk sdk = getProjectDescriptor().getSdk();
        ProjectJdkTable.getInstance().addJdk(sdk);
        ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
      }
    });
  }
}
