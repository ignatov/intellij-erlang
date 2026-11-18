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
  public void testImportedFunctionUsages()        { doTest(3, "importedFunctionUser.erl"); }
  public void testBinaryWidthIsNotAVarDef()       { doTest(2, "binaryWidthIsNotAVarDef.erl");}
  public void testNativeRecordUsagesInSingleFile()    { doTest(3); }
  public void testNativeRecordUsagesInMultipleFiles() { doTest(6, "nativeRecordUsagesInSingleFile.erl"); }
  public void testNativeRecordFieldUsages()           { doTest(3); }
  public void testMapKeyInFunHeadBody()               { doTest(2); }
  public void testMapKeyInFunHeadGuard()              { doTest(2); }
  public void testMapKeyInNestedFunHeads()            { doTest(2); }
  public void testCaseHeadVariable()                  { doTest(4); }
  public void testCaseHeadMatch()                     { doTestFromEachOccurrence(CASE_HEAD_MATCH); }
  public void testCaseHeadMatchInCall()               { doTestFromEachOccurrence(CASE_HEAD_MATCH_IN_CALL); }
  public void testCaseHeadMatchAndClauseMatch()       { doTestFromEachOccurrence(CASE_HEAD_MATCH_AND_CLAUSE_MATCH); }
  public void testCaseHeadRematchOfParameter() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_OF_PARAMETER); }
  public void testCaseHeadRematchOfLocal() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_OF_LOCAL); }
  public void testCaseHeadRematchInFun() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_IN_FUN); }
  public void testCaseHeadRematchOfFunParameter() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_OF_FUN_PARAMETER); }
  public void testCaseHeadRematchInNestedCaseHead() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_IN_NESTED_CASE_HEAD); }
  public void testCaseHeadMatchRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_MATCH_REPEATED_IN_PATTERN); }
  public void testCaseHeadMatchInCallRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_MATCH_IN_CALL_REPEATED_IN_PATTERN); }
  public void testCaseHeadRematchRepeatedInPattern() { doTestFromEachOccurrence(CASE_HEAD_REMATCH_REPEATED_IN_PATTERN); }

//TODO enable these tests when reference search will make use of custom WordsScanner implementations
//  public void testEmptyAtomFunctionInSingleFile()    { doTest(2); }
//  public void testEmptyAtomFunctionInMultipleFiles() { doTest(3, "emptyAtomFunctionInSingleFile.erl"); }

  private static final String CASE_HEAD_MATCH =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of a -> Var; _ when Var =:= b -> ok end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_MATCH_IN_CALL =
    "-module(a).\n-export([foo/1]).\nfoo(D) -> case bar(Var = D) of ok -> Var; _ -> error end.\nbar(X) -> X.\n";
  private static final String CASE_HEAD_MATCH_AND_CLAUSE_MATCH =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of a -> Var = a; _ -> ok end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCH_OF_PARAMETER =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of _ -> Var end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCH_OF_LOCAL =
    "-module(a).\n-export([foo/0]).\nfoo() -> Var = 1, case Var = bar() of _ -> Var end, Var.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCH_IN_FUN =
    "-module(a).\n-export([foo/0]).\nfoo() -> Var = 1, F = fun() -> case Var = bar() of _ -> Var end end, F().\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCH_OF_FUN_PARAMETER =
    "-module(a).\n-export([foo/0]).\nfoo() -> fun(Var) -> case Var = bar() of _ -> Var end end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_REMATCH_IN_NESTED_CASE_HEAD =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of _ -> case Var = bar() of _ -> Var end end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_MATCH_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/0]).\nfoo() -> case Var = bar() of Var when Var > 0 -> Var; _ -> Var end.\nbar() -> 1.\n";
  private static final String CASE_HEAD_MATCH_IN_CALL_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/1]).\nfoo(D) -> case bar(Var = D) of {Var, _} -> Var; _ -> Var end.\nbar(X) -> X.\n";
  private static final String CASE_HEAD_REMATCH_REPEATED_IN_PATTERN =
    "-module(a).\n-export([foo/1]).\nfoo(Var) -> case Var = bar() of Var when Var > 0 -> Var; _ -> Var end, Var.\nbar() -> 1.\n";

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
