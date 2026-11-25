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

package org.intellij.erlang.refactoring;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.BaseRefactoringProcessor;
import com.intellij.refactoring.safeDelete.SafeDeleteHandler;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;

public class ErlangSafeDeleteTest extends LightJavaCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
      return "testData/refactoring/delete/";
    }

  private void doTest() {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    PsiElement element = myFixture.getElementAtCaret();
    SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{element}, false);
    myFixture.checkResultByFile(testName + "-after.erl");
  }

  public void testFunction()    { doTest(); }
  public void testWithExports() { doTest(); }
  public void testRecursiveFunction() { doTest(); }

  public void testQualifiedRecursiveCall() {
    ErlangFile file = (ErlangFile) myFixture.configureByText("a.erl", "-module(a).\nloop(N) -> a:loop(N - 1).\n");
    SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{file.getFunction("loop", 1)}, false);
    assertEmpty(file.getFunctions());
  }

  public void testMutuallyRecursiveFunctionsDeletedTogether() {
    ErlangFile file = (ErlangFile) myFixture.configureByText("a.erl", "-module(a).\nping(N) -> pong(N).\npong(N) -> ping(N).\n");
    SafeDeleteHandler.invoke(myFixture.getProject(), file.getFunctions().toArray(PsiElement.EMPTY_ARRAY), false);
    assertEmpty(file.getFunctions());
  }

  public void testOneOfMutuallyRecursiveFunctions() {
    ErlangFile file = (ErlangFile) myFixture.configureByText("a.erl", "-module(a).\nping(N) -> pong(N).\npong(N) -> ping(N).\n");
    assertConflict(file.getFunction("ping", 1));
  }

  public void testRecursiveFunctionCalledFromOtherFunction() {
    ErlangFile file = (ErlangFile) myFixture.configureByText("a.erl", "-module(a).\nloop(N) -> loop(N - 1).\nmain() -> loop(1).\n");
    assertConflict(file.getFunction("loop", 1));
  }

  private void assertConflict(ErlangFunction function) {
    try {
      SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{function}, false);
      fail("The call from another function must be reported");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("not safe to delete"));
    }
    assertTrue(function.isValid());
  }

  public void testIncludedHeader() {
    PsiFile header = myFixture.addFileToProject("header.hrl", "-define(HEADER, 1).\n");
    myFixture.addFileToProject("includer.erl", "-module(includer).\n-include(\"header.hrl\").\n");
    try {
      SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{header}, false);
      fail("The usage of the header in includer.erl must be reported");
    }
    catch (BaseRefactoringProcessor.ConflictsInTestsException e) {
      assertTrue(e.getMessage(), e.getMessage().contains("header.hrl"));
    }
    assertTrue(header.isValid());
  }

  public void testNotIncludedHeader() {
    PsiFile header = myFixture.addFileToProject("header.hrl", "-define(HEADER, 1).\n");
    myFixture.addFileToProject("includer.erl", "-module(includer).\n-include(\"other.hrl\").\n");
    SafeDeleteHandler.invoke(myFixture.getProject(), new PsiElement[]{header}, false);
    assertFalse(header.isValid());
  }
}
