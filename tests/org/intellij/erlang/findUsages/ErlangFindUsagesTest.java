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

package org.intellij.erlang.findUsages;

import com.intellij.psi.PsiElement;
import com.intellij.usageView.UsageInfo;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangFindUsagesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/find-usages/";
  }

  public void testFunctionUsagesInSingleFile()    { doTest(4); }
  public void testFunctionUsagesInMultipleFiles() { doTest(5, "functionUsagesInSingleFile.erl");}
  public void testBinaryWidthIsNotAVarDef()       { doTest(2, "binaryWidthIsNotAVarDef.erl");}
  public void testNativeRecordUsagesInSingleFile()    { doTest(3); }
  public void testNativeRecordUsagesInMultipleFiles() { doTest(6, "nativeRecordUsagesInSingleFile.erl"); }
  public void testNativeRecordFieldUsages()           { doTest(3); }
  public void testMapKeyInFunHeadBody()               { doTest(2); }
  public void testMapKeyInFunHeadGuard()              { doTest(2); }
  public void testMapKeyInNestedFunHeads()            { doTest(2); }
  public void testCaseHeadVariable()                  { doTest(4); }
  public void testCaseHeadRematchesParameter()        { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_PARAMETER); }
  public void testCaseHeadRematchesBoundVariable()    { doTestFromEachOccurrence(CASE_HEAD_REMATCHES_BOUND_VARIABLE); }
  public void testCaseHeadVariableInNestedReceive()   { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE); }
  public void testCaseHeadVariableRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN); }

//TODO enable these tests when reference search will make use of custom WordsScanner implementations
//  public void testEmptyAtomFunctionInSingleFile()    { doTest(2); }
//  public void testEmptyAtomFunctionInMultipleFiles() { doTest(3, "emptyAtomFunctionInSingleFile.erl"); }

  private static final String CASE_HEAD_REMATCHES_PARAMETER =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of _ -> Var end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCHES_BOUND_VARIABLE =
    "-module(a).\n-export([foo/0]).\nfoo() -> Var = 1, case Var = bar() of _ -> Var end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_IN_NESTED_RECEIVE =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of _ -> receive Var -> Var end end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_VARIABLE_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of Var -> Var end.\nbar() -> 1.\n";

  private void doTestFromEachOccurrence(String text) {
    Set<Integer> expected = new TreeSet<>();
    Matcher matcher = Pattern.compile("\\bVar\\b").matcher(text);
    while (matcher.find()) expected.add(matcher.start());
    for (int offset : expected) {
      myFixture.configureByText("a.erl", text.substring(0, offset) + "<caret>" + text.substring(offset));
      PsiElement target = myFixture.getElementAtCaret();
      Set<Integer> found = new TreeSet<>();
      found.add(target.getTextRange().getStartOffset());
      for (UsageInfo usage : myFixture.findUsages(target)) {
        PsiElement element = usage.getElement();
        if (element != null) found.add(element.getTextRange().getStartOffset());
      }
      assertEquals("Find usages from offset " + offset, expected, found);
    }
  }

  private void doTest(int expectedResult, String ... extraFiles) {
    String[] files = new String[1 + extraFiles.length];
    files[0] = getTestName(true) + ".erl";
    System.arraycopy(extraFiles, 0, files, 1, extraFiles.length);
    assertEquals(expectedResult, myFixture.testFindUsages(files).size());
  }
}
