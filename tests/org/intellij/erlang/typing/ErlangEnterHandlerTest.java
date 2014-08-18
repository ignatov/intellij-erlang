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

package org.intellij.erlang.typing;

import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangEnterHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  @Override
  protected String getTestDataPath() {
    return "testData/autotemplates/";
  }

  public void testBeginEndSimple() throws Throwable                       { doTest(); }
  public void testBeginEndNested() throws Throwable                       { doTest(); }
  public void testBeginEndWithSucceedingExpressions() throws Throwable    { doTest(); }
  public void testBeginEndWithSucceedingTryCatchBlock() throws Throwable  { doTest(); }
  public void testBeginEndWithSucceedingAfterClause() throws Throwable    { doTest(); }

  public void testCaseOfSimple() throws Throwable                         { doTest(); }
  public void testCaseOfWithSucceedingFunctions() throws Throwable        { doTest(); }

  public void testReceiveSimple() throws Throwable                        { doTest(); }
  public void testReceiveWithSucceedingFunctions() throws Throwable       { doTest(); }

  public void testIfSimple() throws Throwable                             { doTest(); }
  public void testIfWithSucceedingFunctions() throws Throwable            { doTest(); }

  public void testTryCatchSimple() throws Throwable                       { doTest(); }
  public void testTryOfSimple() throws Throwable                          { doTest(); }
  public void testTryOfCatchSimple() throws Throwable                     { doTest(); }

  private void doTest() throws Throwable {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.type('\n');
    myFixture.checkResultByFile(getTestName(true) + "-after.erl");
  }
}
