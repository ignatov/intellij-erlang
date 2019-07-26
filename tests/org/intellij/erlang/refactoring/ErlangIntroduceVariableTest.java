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

  private void doTest() { doTest(ALL); }

  private void doTest(ReplaceStrategy replaceStrategy)  {
    String name = getTestName(true);
    myFixture.configureByFile(name + ".erl");
    new ErlangIntroduceVariableHandler(replaceStrategy).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    myFixture.checkResultByFile(name + "-after.erl");
  }
}
