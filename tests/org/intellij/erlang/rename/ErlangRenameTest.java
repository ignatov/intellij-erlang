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

package org.intellij.erlang.rename;

import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangRenameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/rename/";
  }

  public void testUnquotedFunction()  { doTest("bar"); }
  public void testQuotedFunction()    { doTest("bar"); }
  public void testQuoteFunctionName() { doTest("BAR"); }

  public void testUnquotedMacro()     { doTest("bar"); }
  public void testQuotedMacro()       { doTest("bar"); }
  public void testUnderscoreMacro()   { doTest("Foo"); }
  public void testMacrosInAttrs()     { doTest("BAR"); }
  
  public void testMultiTarget()       { doTest("Bar"); }
  public void testMultiTargetMacro()  { doTest("BAR"); }

  public void testMapKeyInFunHeadBody()           { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromKey()    { doTest("Key2"); }
  public void testMapKeyInFunHeadBodyFromBody()   { doTest("Key2"); }
  public void testMapKeyInFunHeadGuard()          { doTest("Key2"); }
  public void testMapKeyInNestedFunHeads()        { doTest("Key2"); }
  public void testMapKeyInNestedFunHeadsFromOuter() { doTest("Key2"); }
  public void testCaseHeadVariable()               { doTest("NewVar"); }
  public void testCaseHeadVariableFromUse()        { doTest("NewVar"); }
  public void testCaseHeadRematchesParameter()     { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_PARAMETER); }
  public void testCaseHeadRematchesBoundVariable() { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_BOUND_VARIABLE); }
  public void testCaseHeadVariableInNestedReceive() { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE); }
  public void testCaseHeadVariableRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN); }
  
  public void testModuleToQuotedName() { doTest("'moduleToQuotedName-after'", true); }
  public void testUnquotedModule()     { doTest("unquotedModule-after", true); }
  public void testQuotedModule()       { doTest("quotedModule-after", true); }

  private static final String CASE_HEAD_REMATCHES_PARAMETER =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of _ -> Var end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCHES_BOUND_VARIABLE =
    "-module(a).\n-export([foo/0]).\nfoo() -> Var = 1, case Var = bar() of _ -> Var end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of _ -> receive Var -> Var end end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of Var -> Var end.\nbar() -> 1.\n";

  private void doTestFromEachOccurrence(String text) {
    List<Integer> offsets = varOffsets(text);
    String expected = text.replaceAll("\\bVar\\b", "NewVar");
    for (int offset : offsets) {
      myFixture.configureByText("a.erl", text.substring(0, offset) + "<caret>" + text.substring(offset));
      myFixture.renameElementAtCaret("NewVar");
      assertEquals("Rename from offset " + offset, expected, myFixture.getEditor().getDocument().getText());
    }
  }

  private static List<Integer> varOffsets(String text) {
    List<Integer> offsets = new ArrayList<>();
    Matcher matcher = Pattern.compile("\\bVar\\b").matcher(text);
    while (matcher.find()) offsets.add(matcher.start());
    return offsets;
  }

  private void doTest(String newName) {
    doTest(newName, false);
  }

  private void doTest(String newName, boolean isRenameModuleTest) {
    myFixture.testRename(getTestName(true) + ".erl", getTestName(true) + "-after.erl", newName);
    if (isRenameModuleTest) {
      assertEquals(StringUtil.unquoteString(newName, '\'') + ".erl", myFixture.getFile().getName());
    }
  }
}
