/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.sdk;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiManager;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

/**
 * The lexer makes {@code maybe} a keyword or an atom from the release of the project SDK. After the project SDK changes
 * across OTP 25, a loaded file must have the tree that a fresh parse with the new SDK gives (issue #1326).
 */
public class ErlangMaybeSdkSwitchTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R18/", ErlangSdkRelease.V_18_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testFunctionNamedMaybeAfterSdkSwitch() {
    String text = "-module(a).\nmaybe() -> ok.\n";
    myFixture.configureByText("a.erl", text);
    assertEquals(1, ((ErlangFile) myFixture.getFile()).getFunctions().size());

    switchProjectSdk(ErlangSdkType.createMockSdk("testData/mockSdk-R25/", ErlangSdkRelease.V_25_0));

    assertTreeOfFreshParse(text);
  }

  public void testMaybeExpressionAfterSdkSwitch() {
    String text = "-module(a).\nf() -> maybe ok ?= ok end.\n";
    myFixture.configureByText("a.erl", text);

    switchProjectSdk(ErlangSdkType.createMockSdk("testData/mockSdk-R25/", ErlangSdkRelease.V_25_0));

    assertTreeOfFreshParse(text);
  }

  private void switchProjectSdk(@NotNull Sdk sdk) {
    ApplicationManager.getApplication().runWriteAction(() -> {
      ProjectJdkTable.getInstance().addJdk(sdk);
      ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
    });
    assertTrue(ErlangSdkType.getRelease(getProject()).erlangFeatureMaybe());
  }

  private void assertTreeOfFreshParse(@NotNull String text) {
    VirtualFile vFile = myFixture.getFile().getVirtualFile();
    PsiFile loaded = PsiManager.getInstance(getProject()).findFile(vFile);
    assertNotNull(loaded);
    PsiFile fresh = PsiFileFactory.getInstance(getProject()).createFileFromText("a.erl", ErlangFileType.MODULE, text);
    assertEquals(DebugUtil.psiToString(fresh, true), DebugUtil.psiToString(loaded, true));
  }
}
