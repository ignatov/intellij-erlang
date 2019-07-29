/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.refactoring;

import com.intellij.psi.PsiElement;
import com.intellij.refactoring.safeDelete.SafeDeleteHandler;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;

public class ErlangSafeDeleteTest extends LightJavaCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
      return "testData/refactoring/delete/";
    }

  private void doTest() {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    PsiElement element = myFixture.getElementAtCaret();
    SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{element}, false);
    myFixture.checkResultByFile(testName + "-after.erl");
  }

  public void testFunction()    { doTest(); }
  public void testWithExports() { doTest(); }
}
