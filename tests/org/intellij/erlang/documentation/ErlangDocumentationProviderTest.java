/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.documentation;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.intellij.erlang.ErlangDocumentationProvider;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("ConstantConditions")
public class ErlangDocumentationProviderTest extends LightCodeInsightFixtureTestCase {
  private ErlangDocumentationProvider myErlangDocProvider;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    myErlangDocProvider = new ErlangDocumentationProvider();
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
      }
    };
  }

  private void doTest(@NotNull String expected, @NotNull String text) throws Exception {
    myFixture.configureByText("test.erl", text);
    final int caretPosition = myFixture.getEditor().getCaretModel().getOffset();
    final PsiReference psiReference = myFixture.getFile().findReferenceAt(caretPosition);
    final PsiElement resolve = psiReference.resolve();
    assertNotNull(resolve);
    assertEquals(expected, myErlangDocProvider.getUrlFor(resolve, null).get(0));
  }

  public void testFunctionExternalUrl() throws Exception {
    doTest("http://www.erlang.org/documentation/doc-5.9.2/lib/stdlib-1.18.2/doc/html/lists.html#foreach-2",
      "-module(test).\n" +
      "test() ->\n" +
      "    lists:for<caret>each(foo, bar).\n");
  }

  public void testModuleExternalUrl() throws Exception {
    doTest("http://www.erlang.org/documentation/doc-5.9.2/lib/stdlib-1.18.2/doc/html/lists.html",
      "-module(test).\n" +
      "test() ->\n" +
      "    lis<caret>ts:foreach(foo, bar).\n");
  }
}