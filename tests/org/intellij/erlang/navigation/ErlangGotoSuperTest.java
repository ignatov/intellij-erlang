/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.navigation;

import com.intellij.lang.CodeInsightActions;
import com.intellij.lang.LanguageCodeInsightActionHandler;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangCallbackSpec;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

import java.util.List;

public class ErlangGotoSuperTest extends LightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/navigation/goto_super/";
  }

  public void testSingleFunction() throws Throwable {
    doTest(1, getTestName(true) + ".erl", "test_behaviour.erl");
  }

  public void testNotSpecifiedBehavioursAreNotSuggested() throws Throwable {
    doTest(1, "singleFunction.erl", "test_behaviour.erl", "test_behaviour2.erl");
  }

  public void testTwoFunctions() throws Throwable {
    doTest(2, getTestName(true) + ".erl", "test_behaviour.erl", "test_behaviour2.erl");
  }

  private void doTest(int expectedCallbacksCount, String... filesToLoad) throws Throwable {
    myFixture.configureByFiles(filesToLoad);

    LanguageCodeInsightActionHandler handler = CodeInsightActions.GOTO_SUPER.forLanguage(ErlangLanguage.INSTANCE);
    assertNotNull("GotoSuperHandler for Erlang was not found.", handler);

    PsiElement focusedElement = myFixture.getFile().findElementAt(myFixture.getEditor().getCaretModel().getOffset());
    ErlangFunction function = PsiTreeUtil.getParentOfType(focusedElement, ErlangFunction.class);

    assertNotNull("Invalid test data. A caret should be placed to function.", function);

    List<ErlangCallbackSpec> callbackSpecFuns = ErlangNavigationUtil.getCallbackSpecs(function);

    assertEquals("Unexpected callbacks count.", expectedCallbacksCount, callbackSpecFuns.size());

    for (ErlangCallbackSpec callbackSpecFun : callbackSpecFuns) {
      assertEquals("Unexpected function signature.",
        ErlangPsiImplUtil.createFunctionPresentation(function),
        ErlangPsiImplUtil.createFunctionPresentationFromCallbackSpec(callbackSpecFun));
    }
  }
}
