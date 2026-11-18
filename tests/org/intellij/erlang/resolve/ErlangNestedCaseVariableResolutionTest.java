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

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.psi.impl.ResolveUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

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

  public void testBindingInSelectorOfNestedCase() {
    doTest("f() -> case a of _ -> case Y = g() of _ -> Y end end.", 2, 1);
  }

  public void testBindingInTupleMatchInSelectorOfNestedCase() {
    doTest("f() -> case a of _ -> case {ok, Y} = g() of {ok, _} -> Y end end.", 2, 1);
  }

  public void testBindingInArgumentOfSelectorOfNestedCase() {
    doTest("f(L) -> case a of _ -> case lists:keyfind(k, 1, Y = L) of false -> Y; T -> {T, Y} end end.", 3, 1);
  }

  public void testBindingInSelectorOfNestedCaseInGuard() {
    doTest("f() -> case a of _ -> case Y = g() of Z when Z > Y -> Y end end.", 2, 1);
  }

  public void testBindingInBlockOfSelectorOfNestedCase() {
    doTest("f() -> case a of _ -> case begin Y = g(), Y end of _ -> Y end end.", 3, 1);
  }

  public void testBindingInPatternOfEarlierNestedCase() {
    doTest("f() -> case a of _ -> case g() of Y -> ok end, Y end.", 2, 1);
  }

  public void testBindingInSelectorOfEarlierNestedCase() {
    doTest("f() -> case a of _ -> case Y = g() of _ -> ok end, Y end.", 2, 1);
  }

  public void testRenameBindingInSelectorOfNestedCase() {
    myFixture.configureByText("nested_cases.erl", "f() -> case a of _ -> case Y<caret> = g() of _ -> case Y of _ -> Y end end end.");
    myFixture.renameElementAtCaret("Zz");
    myFixture.checkResult("f() -> case a of _ -> case Zz<caret> = g() of _ -> case Zz of _ -> Zz end end end.");
  }

  public void testRenameBindingInPatternOfEarlierNestedCase() {
    myFixture.configureByText("nested_cases.erl", "f() -> case a of _ -> case g() of Y<caret> -> ok end, Y end.");
    myFixture.renameElementAtCaret("Zz");
    myFixture.checkResult("f() -> case a of _ -> case g() of Zz<caret> -> ok end, Zz end.");
  }

  public void testCompletionOfBindingInSelectorOfNestedCase() {
    myFixture.configureByText("nested_cases.erl", "f() -> case a of _ -> case Foo = g() of _ -> Fo<caret> end end.");
    myFixture.completeBasic();
    List<String> variants = myFixture.getLookupElementStrings();
    if (variants == null) myFixture.checkResult("f() -> case a of _ -> case Foo = g() of _ -> Foo<caret> end end.");
    else assertContainsElements(variants, "Foo");
  }

  public void testRenameArgumentMatchedInPatternOfEarlierCase() {
    doRenameTest("f(Y<caret>) -> case g() of {ok, Y} -> a; _ -> b end, h(Y).",
                 "f(Zz<caret>) -> case g() of {ok, Zz} -> a; _ -> b end, h(Zz).");
  }

  public void testRenameBindingMatchedInPatternOfEarlierCase() {
    doRenameTest("f() -> Y<caret> = g(), case g() of Y -> ok; _ -> no end, Y.",
                 "f() -> Zz<caret> = g(), case g() of Zz -> ok; _ -> no end, Zz.");
  }

  public void testRenameArgumentMatchedInListPatternOfEarlierCase() {
    doRenameTest("f(Y<caret>) -> case g() of [Y | _] -> ok; [] -> ok end, Y.",
                 "f(Zz<caret>) -> case g() of [Zz | _] -> ok; [] -> ok end, Zz.");
  }

  public void testRenameArgumentMatchedInRecordPatternOfEarlierCase() {
    doRenameTest("f(Y<caret>) -> case g() of #r{a = Y} -> ok end, Y.",
                 "f(Zz<caret>) -> case g() of #r{a = Zz} -> ok end, Zz.");
  }

  public void testRenameArgumentUsedAsMapKeyInPatternOfEarlierCase() {
    doRenameTest("f(K<caret>) -> case g() of #{K := V} -> V end, K.",
                 "f(Zz<caret>) -> case g() of #{Zz := V} -> V end, Zz.");
  }

  public void testRenameArgumentUsedAsBinarySizeInPatternOfEarlierCase() {
    doRenameTest("f(Y<caret>) -> case g() of <<_:Y>> -> ok end, Y.",
                 "f(Zz<caret>) -> case g() of <<_:Zz>> -> ok end, Zz.");
  }

  public void testRenameArgumentMatchedInPatternsOfTwoEarlierCases() {
    doRenameTest("f(Y<caret>) -> case g() of Y -> a end, case h() of Y -> b end, Y.",
                 "f(Zz<caret>) -> case g() of Zz -> a end, case h() of Zz -> b end, Zz.");
  }

  public void testRenameArgumentMatchedInSelector() {
    doRenameTest("f(Y<caret>) -> case {ok, Y} = g() of _ -> Y end.",
                 "f(Zz<caret>) -> case {ok, Zz} = g() of _ -> Zz end.");
  }

  public void testFindUsagesOfArgumentMatchedInPatternOfEarlierCase() {
    myFixture.configureByText("nested_cases.erl", "f(Y) -> case g() of {ok, Y} -> a; _ -> b end, h(Y).");
    assertEquals(2, myFixture.findUsages(findVars("Y").get(0)).size());
  }

  public void testRenameBindingMatchedInPatternOfNestedCase() {
    doRenameTest("f() -> case a of _ -> ok end, case b of _ -> Y<caret> = 1, case c of Y -> Y end end.",
                 "f() -> case a of _ -> ok end, case b of _ -> Zz<caret> = 1, case c of Zz -> Zz end end.");
  }

  public void testEnclosingSelectorsWithoutBindingAreNotSearched() {
    configureNestedCases(30);
    List<ErlangQVar> vars = findVars("X");
    int[] offered = {0};
    ResolveUtil.treeWalkUp(vars.get(vars.size() - 1), new PsiScopeProcessor() {
      @Override
      public boolean execute(@NotNull PsiElement element, @NotNull ResolveState state) {
        if (element instanceof ErlangQVar && "X".equals(element.getText())) offered[0]++;
        return true;
      }
    });
    assertEquals(1, offered[0]);
  }

  public void testFindUsagesAndRenameInNestedCases() {
    configureNestedCases(20);
    List<ErlangQVar> vars = findVars("X");
    assertEquals(20, myFixture.findUsages(vars.get(0)).size());
    myFixture.renameElement(vars.get(0), "Y");
    assertEquals(0, findVars("X").size());
    assertEquals(21, findVars("Y").size());
  }

  private void doRenameTest(String before, String after) {
    myFixture.configureByText("nested_cases.erl", before);
    myFixture.renameElementAtCaret("Zz");
    myFixture.checkResult(after);
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
