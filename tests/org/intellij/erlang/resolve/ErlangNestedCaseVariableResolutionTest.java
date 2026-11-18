/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.resolve;

import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangNestedCaseVariableResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  /**
   * Every reference of {@code X} walks up through the enclosing cases, so resolving all of them is at most
   * quadratic in the nesting: doubling the nesting may make it about four times slower, not sixteen.
   */
  public void testResolveInNestedCasesScalesQuadratically() {
    resolveAllInNestedCases(20);
    long smallNanos = resolveAllInNestedCases(30);
    long largeNanos = resolveAllInNestedCases(60);
    assertTrue("Resolving the variables of 60 nested cases took " + largeNanos / 1_000_000 + " ms after " +
               smallNanos / 1_000_000 + " ms for 30 nested cases",
               largeNanos < smallNanos * 8);
  }

  public void testVariableOfEnclosingClause() {
    doTest("f() -> case a of _ -> Y = 1, case b of _ -> Y end end.", 2, 1);
  }

  public void testVariableOfAllClausesOfEarlierNestedCase() {
    doTest("f(A) -> case A of _ -> case A of 1 -> Y = 1; _ -> Y = 2 end, Y end.", 3, 1, 2);
  }

  public void testMatchLaterInEnclosingClauseDoesNotHideArgument() {
    doTest("f(Y) -> case a of _ -> g(Y), Y = 1 end.", 2, 1);
  }

  public void testFindUsagesAndRenameInNestedCases() {
    configureNestedCases(20);
    List<ErlangQVar> vars = findVars("X");
    assertEquals(20, myFixture.findUsages(vars.get(0)).size());
    myFixture.renameElement(vars.get(0), "Y");
    assertEquals(0, findVars("X").size());
    assertEquals(21, findVars("Y").size());
  }

  private void doTest(String function, int occurrence, Integer... expected) {
    myFixture.configureByText("nested_cases.erl", "-module(nested_cases).\n-export([f/1]).\n" + function + "\n");
    List<ErlangQVar> vars = findVars("Y");
    PsiPolyVariantReference reference = (PsiPolyVariantReference) vars.get(occurrence - 1).getReference();
    List<Integer> targets = new ArrayList<>();
    for (ResolveResult result : reference.multiResolve(false)) {
      targets.add(vars.indexOf(result.getElement()) + 1);
    }
    assertEquals(List.of(expected), targets);
  }

  private void configureNestedCases(int nesting) {
    myFixture.configureByText("nested_cases.erl",
                              "-module(nested_cases).\n-export([f/0]).\nf() -> X = 1, " +
                              "case X of _ -> ".repeat(nesting) + "ok" + " end".repeat(nesting) + ".\n");
  }

  private List<ErlangQVar> findVars(String name) {
    List<ErlangQVar> vars = new ArrayList<>();
    for (ErlangQVar var : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class)) {
      if (name.equals(var.getText())) vars.add(var);
    }
    return vars;
  }

  private long resolveAllInNestedCases(int nesting) {
    configureNestedCases(nesting);
    List<ErlangQVar> vars = findVars("X");
    assertEquals(nesting + 1, vars.size());
    List<PsiReference> references = new ArrayList<>();
    for (ErlangQVar var : vars.subList(1, vars.size())) references.add(var.getReference());

    long start = System.nanoTime();
    for (PsiReference reference : references) {
      assertEquals(vars.get(0), reference.resolve());
    }
    return System.nanoTime() - start;
  }
}
