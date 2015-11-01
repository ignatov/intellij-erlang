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

package org.intellij.erlang.typing;

import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangEnterHandlerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {

  @Override
  protected String getTestDataPath() {
    return "testData/autotemplates/";
  }

  public void testBeginEndSimple()                       { doTest(); }
  public void testBeginEndNested()                       { doTest(); }
  public void testBeginEndWithSucceedingExpressions()    { doTest(); }
  public void testBeginEndWithSucceedingTryCatchBlock()  { doTest(); }
  public void testBeginEndWithSucceedingAfterClause()    { doTest(); }
  public void testBeginEndWithSpace()                    { doTest(); }

  public void testCaseOfSimple()                         { doTest(); }
  public void testCaseOfWithSucceedingFunctions()        { doTest(); }
  public void testCaseOfWithSpace()                      { doTest(); }

  public void testReceiveSimple()                        { doTest(); }
  public void testReceiveWithSucceedingFunctions()       { doTest(); }
  public void testReceiveWithSpace()                     { doTest(); }

  public void testIfSimple()                             { doTest(); }
  public void testIfWithSucceedingFunctions()            { doTest(); }
  public void testIfWithSpace()                          { doTest(); }

  public void testTryCatchSimple()                       { doTest(); }
  public void testTryOfSimple()                          { doTest(); }
  public void testTryOfCatchSimple()                     { doTest(); }
  public void testTryOfWithSpace()                       { doTest(); }

  public void testUnmatchedCurlyBrace()                  { doTest(); }

  public void testEnterBetweenBrackets()                 { doTest(); }
  public void testEnterBetweenCurlies()                  { doTest(); }
  public void testEnterBetweenParens()                   { doTest(); }

  public void testEnterInComment()                       { doTest(); }
  public void testEnterInFunctionDocComment()            { doTest(); }
  public void testEnterInModuleDocComment()              { doTest(); }
  public void testEnterInShebang()                       { doTest(); }

  private void doTest() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.type('\n');
    myFixture.checkResultByFile(getTestName(true) + "-after.erl", true);
  }
}
