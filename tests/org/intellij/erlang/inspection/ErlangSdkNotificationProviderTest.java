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

package org.intellij.erlang.inspection;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationEx;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.sdk.ErlangSdkForSmallIdes;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

public class ErlangSdkNotificationProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangSdkNotificationProviderTest() {
  }

  protected ErlangSdkNotificationProviderTest(boolean isSmallIde) {
    super(isSmallIde);
  }

  // the light project of this test has no SDK on the project or the module
  public void testBannerShownWithoutSdk() {
    VirtualFile file = myFixture.configureByText("a.erl", "-module(a).\n").getVirtualFile();
    assertNotNull("no 'Project SDK is not defined' banner for an .erl file without an Erlang SDK", collectNotificationData(file));
  }

  public void testNoBannerWithSdk() {
    String sdkHome = "testData/mockSdk-R15B02/";
    Sdk sdk = ErlangSdkType.createMockSdk(sdkHome, ErlangSdkRelease.V_R15B02);
    try {
      if (ErlangSystemUtil.isSmallIde()) {
        ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), sdkHome);
      }
      else {
        ApplicationManager.getApplication().runWriteAction(() -> {
          ProjectJdkTable.getInstance().addJdk(sdk);
          ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
        });
      }
      VirtualFile file = myFixture.configureByText("a.erl", "-module(a).\n").getVirtualFile();
      assertNull("a 'Project SDK is not defined' banner for an .erl file with an Erlang SDK", collectNotificationData(file));
    }
    finally {
      if (ErlangSystemUtil.isSmallIde()) {
        ErlangSdkForSmallIdes.setUpOrUpdateSdk(getProject(), "");
      }
      else {
        ApplicationManager.getApplication().runWriteAction(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(null));
      }
    }
  }

  // the provider returns null in unit test mode, so it is called with an application that is not in unit test mode
  private Object collectNotificationData(VirtualFile file) {
    Application application = ApplicationManager.getApplication();
    Application notInUnitTestMode = (Application)Proxy.newProxyInstance(
      application.getClass().getClassLoader(), new Class<?>[]{ApplicationEx.class}, (proxy, method, args) -> {
        if (method.getName().equals("isUnitTestMode") && method.getParameterCount() == 0) return false;
        try {
          return method.invoke(application, args);
        }
        catch (InvocationTargetException e) {
          throw e.getCause();
        }
      });
    Disposable disposable = Disposer.newDisposable();
    try {
      ApplicationManager.setApplication(notInUnitTestMode, disposable);
      return new ErlangSdkNotificationProvider().collectNotificationData(getProject(), file);
    }
    finally {
      Disposer.dispose(disposable);
    }
  }
}
