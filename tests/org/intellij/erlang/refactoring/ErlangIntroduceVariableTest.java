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
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler;

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

  public void testExpressionUsingGeneratorVariable() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    try {
      new ErlangIntroduceVariableHandler(ALL).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    }
    catch (CommonRefactoringUtil.RefactoringErrorHintException ignored) {
      // refusing the refactoring is a correct outcome
    }
    String text = myFixture.getEditor().getDocument().getText();
    String beforeComprehension = text.substring(0, text.indexOf('['));
    assertFalse("X is bound only by the generator, but the declaration was put before the comprehension:\n" + text,
                beforeComprehension.contains("X * 2"));
  }

  public void testExpressionUsingGeneratorVariableInFilter() { doTestRefused(); }
  public void testExpressionUsingBinaryGeneratorVariable()   { doTestRefused(); }
  public void testExpressionUsingVariableBoundInSameStatement(){ doTestRefused(); }
  public void testGeneratorSourceExtracted()                 { doTest(); }
  public void testFunClauseVariable()                        { doTest(); }

  private void doTestRefused() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    String before = myFixture.getEditor().getDocument().getText();
    try {
      new ErlangIntroduceVariableHandler(ALL).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
      fail("The refactoring must be refused");
    }
    catch (CommonRefactoringUtil.RefactoringErrorHintException e) {
      assertEquals("Selected expression uses variables that are not bound where the new variable would be declared", e.getMessage());
    }
    assertEquals(before, myFixture.getEditor().getDocument().getText());
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
