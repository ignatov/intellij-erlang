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
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.util.PlatformUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

public abstract class ErlangLightPlatformCodeInsightFixtureTestCase extends BasePlatformTestCase {
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

    releaseErlangSdks();

    super.tearDown();
  }

  private static void releaseErlangSdks() {
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable table = ProjectJdkTable.getInstance();
      List<Sdk> sdksOfType = table.getSdksOfType(ErlangSdkType.getInstance());
      for (Sdk sdk : sdksOfType) {
        table.removeJdk(sdk);
      }
    });
  }

  protected void setUpProjectSdk() {
    ApplicationManager.getApplication().runWriteAction(() -> {
      Sdk sdk = getProjectDescriptor().getSdk();
      if (sdk != null) {
        ProjectJdkTable instance = ProjectJdkTable.getInstance();
        var prev = instance.findJdk(sdk.getName(), sdk.getSdkType().getName());
        if (prev != null) {
          instance.removeJdk(prev);
        }
        instance.addJdk(sdk);
        ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
      }
    });
  }

  protected void launchIntention(@NotNull String name) {
    var availableIntentions = myFixture.filterAvailableIntentions(name);
    var action = ContainerUtil.getFirstItem(availableIntentions);

    assertNotNull(action);
    myFixture.launchAction(action);
  }

  protected void assertNoIntentionsAvailable(@NotNull String name, @Nullable String message) {
    var availableIntentions = myFixture.filterAvailableIntentions(name);
    var action = ContainerUtil.getFirstItem(availableIntentions);

    assertNull(message, action);
  }
  
  protected void assertNoIntentionsAvailable(@NotNull String name) {
    assertNoIntentionsAvailable(name, null);
  }

  @NotNull
  protected <T extends PsiElement> T getElementAtCaret(@NotNull Class<T> clazz) {
    int offset = myFixture.getEditor().getCaretModel().getOffset();
    PsiElement focused = myFixture.getFile().findElementAt(offset);
    return Objects.requireNonNull(PsiTreeUtil.getParentOfType(focused, clazz));
  }
}
