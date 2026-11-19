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

package org.intellij.erlang.inspection;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangConditionalBranchesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testCachedByPsiModificationCount() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("CachedBranches.erl", """
      -ifdef(X).
      f() -> ok.
      -else.
      f() -> ok.
      -endif.
      """);
    ErlangConditionalBranches branches = ErlangConditionalBranches.getInstance(file);
    assertSame(branches, ErlangConditionalBranches.getInstance(file));
    assertTrue(branches.areExclusive(file.getFunctions().get(0), file.getFunctions().get(1)));

    WriteCommandAction.runWriteCommandAction(getProject(), () -> myFixture.getEditor().getDocument().setText("""
      -ifdef(X).
      f() -> ok.
      -endif.
      f() -> ok.
      """));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();

    ErlangConditionalBranches updated = ErlangConditionalBranches.getInstance(file);
    assertNotSame(branches, updated);
    assertFalse(updated.areExclusive(file.getFunctions().get(0), file.getFunctions().get(1)));
  }
}
