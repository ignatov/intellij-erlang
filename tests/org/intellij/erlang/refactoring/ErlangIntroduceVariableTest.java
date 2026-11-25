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

package org.intellij.erlang.refactoring;

import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler;

import java.util.List;

import static org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler.ReplaceStrategy;
import static org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler.ReplaceStrategy.ALL;
import static org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler.ReplaceStrategy.SINGLE;

public class ErlangIntroduceVariableTest extends BasePlatformTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/refactoring/introduce_variable";
  }

  public void testSimple()                  { doTest(); }
  public void testCaseExpression()          { doTest(); }
  public void testFunctionArguments()       { doTest(); }
  public void testFunctionClauseScope()     { doTest(); }
  public void testParenthesesElimination()  { doTest(); }
  public void testSingleOccurrenceReplace() { doTest(SINGLE); }
  public void testCaseExpressionRenamed()   { doTestWithRename("C"); }
  public void testIfExpressionRenamed()     { doTestWithRename("Sign"); }
  public void testLeadingRunOfChain()       { doTest(SINGLE); }
  public void testLeadingRunOfChainInCall() { doTest(SINGLE); }
  public void testLeadingRunOccurrences()   { doTest(); }
  public void testChainOccurrenceInLongerChain() { doTest(); }
  public void testLeadingRunOperatorSelected() { doTest(SINGLE); }
  public void testLeadingRunAlsoWhole()     { doTest(); }

  public void testSmartIntroduceTargetsInChain() {
    myFixture.configureByText("a.erl", "f(A, B, C) -> A + <caret>B + C.");
    PsiElement element = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    assertEquals(List.of("B", "A + B", "A + B + C"),
                 ContainerUtil.map(ErlangRefactoringUtil.getIntroduceTargets(element), ErlangRefactoringUtil.Occurrence::getText));
  }

  public void testSmartIntroduceLeadingRun() {
    myFixture.configureByText("a.erl", "f(A, B, C) -> foo(A <caret>+ B + C).");
    new ErlangIntroduceVariableHandler(SINGLE).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    myFixture.checkResult("f(A, B, C) -> AB = A + B,\n  foo(AB + C).");
  }

  private void doTest() { doTest(ALL); }

  private void doTestWithRename(String newName) {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
    String name = getTestName(true);
    myFixture.configureByFile(name + ".erl");
    new ErlangIntroduceVariableHandler(ALL).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    TemplateState state = TemplateManagerImpl.getTemplateState(myFixture.getEditor());
    assertNotNull(state);
    TextRange range = state.getCurrentVariableRange();
    assertNotNull(range);
    WriteCommandAction.runWriteCommandAction(myFixture.getProject(), () ->
      myFixture.getEditor().getDocument().replaceString(range.getStartOffset(), range.getEndOffset(), newName));
    state.gotoEnd(false);
    myFixture.checkResultByFile(name + "-after.erl");
  }

  private void doTest(ReplaceStrategy replaceStrategy)  {
    String name = getTestName(true);
    myFixture.configureByFile(name + ".erl");
    new ErlangIntroduceVariableHandler(replaceStrategy).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    myFixture.checkResultByFile(name + "-after.erl");
  }
}
