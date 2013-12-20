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

package org.intellij.erlang.refactoring;

import com.intellij.psi.PsiElement;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.intellij.erlang.ErlangInlineVariableHandler;
import org.intellij.erlang.psi.ErlangQVar;

public class ErlangInlineVariableTest extends LightCodeInsightFixtureTestCase {
  public void testSimple()                             throws Exception { doTest(); }
  public void testFunExpression()                      throws Exception { doTest(); }
  public void testLowPrecedence()                      throws Exception { doTest(); }
  public void testCaseExpression()                     throws Exception { doTest(); }
  public void testNestedCaseFormatting()               throws Exception { doTest(); }
  public void testFunExpressionInline1()               throws Exception { doTest(); }
  public void testFunExpressionInline2()               throws Exception { doTest(); }
  public void testFunctionCallSubstitution()           throws Exception { doTest(); }
  public void testFunctionCallSubstitutionWithArity1() throws Exception { doTest(); }
  public void testFunctionCallSubstitutionWithArity2() throws Exception { doTest(); }

  @Override
  protected String getTestDataPath() {
    return "testData/inline/";
  }

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  private void doTest() {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    PsiElement element = myFixture.getElementAtCaret();
    assertInstanceOf(element, ErlangQVar.class);
    new ErlangInlineVariableHandler().inlineElement(myFixture.getProject(), myFixture.getEditor(), element);
    myFixture.checkResultByFile(testName + "-after.erl");
  }
}
