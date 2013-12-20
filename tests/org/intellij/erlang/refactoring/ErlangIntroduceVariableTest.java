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

import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.intellij.erlang.refactor.introduce.ErlangIntroduceVariableHandler;

public class ErlangIntroduceVariableTest extends LightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/refactoring/introduce_variable";
  }

  public void testSimple()                  throws Throwable { doTest(ErlangIntroduceVariableHandler.ReplaceStrategy.ALL); }
  public void testFunctionArguments()       throws Throwable { doTest(ErlangIntroduceVariableHandler.ReplaceStrategy.ALL); }
  public void testFunctionClauseScope()     throws Throwable { doTest(ErlangIntroduceVariableHandler.ReplaceStrategy.ALL); }
  public void testSingleOccurrenceReplace() throws Throwable { doTest(ErlangIntroduceVariableHandler.ReplaceStrategy.SINGLE); }
  public void testParenthesesElimination()  throws Throwable { doTest(ErlangIntroduceVariableHandler.ReplaceStrategy.ALL); }

  private void doTest(ErlangIntroduceVariableHandler.ReplaceStrategy replaceStrategy) throws Throwable {
    myFixture.configureByFile(getTestName(true) + ".erl");
    new ErlangIntroduceVariableHandler(replaceStrategy).invoke(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile(), null);
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }
}
