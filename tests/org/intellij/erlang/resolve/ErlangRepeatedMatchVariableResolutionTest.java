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

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangRepeatedMatchVariableResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String REPEATED_PAIR = """
        X = a,
        case A of 1 -> X = a; _ -> X = a end,
    """;

  // #1433: the last X is bound only by the first X = a; every later X only matches it again
  public void testVariableMatchedAgainInCaseClausesHasOneTarget() {
    List<ErlangQVar> vars = configure(10);
    ErlangQVar first = vars.get(0);
    ErlangQVar last = vars.get(vars.size() - 1);
    ResolveResult[] results = multiResolve(last);
    assertEquals(1, results.length);
    assertEquals(first, results[0].getElement());
  }

  // #1433: eight more repetitions must not multiply the time of the resolve by 2^8
  public void testResolveAfterRepeatedMatchesDoesNotDouble() {
    assertResolveDoesNotDouble("");
  }

  // #1481: a busy machine that pauses one resolve must not fail the check of testResolveAfterRepeatedMatchesDoesNotDouble
  // although the resolve does not double
  public void testDoublingCheckHoldsOnBusyMachine() throws InterruptedException {
    List<Thread> load = new ArrayList<>();
    for (int i = 0; i < 3 * Runtime.getRuntime().availableProcessors(); i++) {
      Thread t = new Thread(() -> { while (!Thread.currentThread().isInterrupted()) Thread.onSpinWait(); });
      t.setDaemon(true);
      t.start();
      load.add(t);
    }
    try {
      for (int i = 0; i < 30; i++) {
        assertResolveDoesNotDouble("run " + i + ": ");
      }
    }
    finally {
      for (Thread t : load) t.interrupt();
      for (Thread t : load) t.join();
    }
  }

  // a variable bound in every clause of a case and matched again after it resolves to each clause binding once
  public void testMatchAfterCaseResolvesToEveryClauseBinding() {
    myFixture.configureByText("rep.erl", """
      -module(rep).
      -export([f/1]).
      f(A) ->
          case A of 1 -> X = a; 2 -> X = b; _ -> X = c end,
          X = a,
          X = a,
          X.
      """);
    List<ErlangQVar> vars = new ArrayList<>(PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class));
    vars.removeIf(v -> !"X".equals(v.getText()));
    assertEquals(6, vars.size());
    List<Object> targets = new ArrayList<>();
    for (ResolveResult r : multiResolve(vars.get(5))) targets.add(r.getElement());
    assertEquals(vars.subList(0, 3), targets);
  }

  // #1503: resolving every variable of the clause grows linearly with the number of matches, not quadratically.
  // From 25 to 200 repetitions the total grows about 8x when it is linear and 64x when it is quadratic
  public void testResolvingEveryVariableOfClauseIsNotQuadratic() {
    long small = fastestResolveOfEveryVariable(25);
    long big = fastestResolveOfEveryVariable(200);
    assertTrue("25 repetitions: " + small + " ns, 200 repetitions: " + big + " ns, ratio " + (double) big / Math.max(small, 1L),
               big < 24 * Math.max(small, 1_000_000L));
  }

  // #1503: the earlier bindings that resolves share are dropped when the file changes
  public void testResolveAfterEditDropsEarlierBindings() {
    myFixture.configureByText("rep.erl", """
      -module(rep).
      -export([f/0]).
      f() ->
          X = a,
          X = b,
          X = c,
          X.
      """);
    List<ErlangQVar> vars = xVars();
    assertEquals(vars.get(0), multiResolve(vars.get(3))[0].getElement());
    assertEquals(vars.get(0), multiResolve(vars.get(2))[0].getElement());
    Document document = myFixture.getEditor().getDocument();
    int start = document.getText().indexOf("X = a,");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      document.deleteString(start, start + "X = a,".length());
      PsiDocumentManager.getInstance(getProject()).commitDocument(document);
    });
    vars = xVars();
    assertEquals(3, vars.size());
    ResolveResult[] results = multiResolve(vars.get(2));
    assertEquals(1, results.length);
    assertEquals(vars.get(0), results[0].getElement());
  }

  private List<ErlangQVar> xVars() {
    List<ErlangQVar> vars = new ArrayList<>(PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class));
    vars.removeIf(v -> !"X".equals(v.getText()));
    return vars;
  }

  private long fastestResolveOfEveryVariable(int repetitions) {
    List<ErlangQVar> vars = configure(repetitions);
    for (ErlangQVar v : vars) multiResolve(v);
    long fastest = Long.MAX_VALUE;
    for (int i = 0; i < 3; i++) {
      long start = System.nanoTime();
      for (ErlangQVar v : vars) multiResolve(v);
      fastest = Math.min(fastest, System.nanoTime() - start);
    }
    return fastest;
  }

  // the fastest of several warmed-up resolves of each size, so that a pause of the machine during one resolve does not count.
  // The resolve grows about 2x from 8 to 16 repetitions and 256x when it doubles, so the 32x limit leaves room for a busy machine
  private void assertResolveDoesNotDouble(String prefix) {
    long small = fastestResolve(8);
    long big = fastestResolve(16);
    assertTrue(prefix + "8 repetitions: " + small + " ns, 16 repetitions: " + big + " ns", big < 32 * Math.max(small, 1_000_000L));
  }

  private long fastestResolve(int repetitions) {
    List<ErlangQVar> vars = configure(repetitions);
    ErlangQVar last = vars.get(vars.size() - 1);
    for (int i = 0; i < 5; i++) multiResolve(last);
    long fastest = Long.MAX_VALUE;
    for (int i = 0; i < 10; i++) {
      long start = System.nanoTime();
      multiResolve(last);
      fastest = Math.min(fastest, System.nanoTime() - start);
    }
    return fastest;
  }

  private static ResolveResult[] multiResolve(ErlangQVar var) {
    return ((PsiPolyVariantReference) var.getReference()).multiResolve(false);
  }

  private List<ErlangQVar> configure(int repetitions) {
    String text = "-module(rep).\n-export([f/1]).\nf(A) ->\n" + REPEATED_PAIR.repeat(repetitions) + "    X.\n";
    myFixture.configureByText("rep.erl", text);
    List<ErlangQVar> vars = new ArrayList<>();
    for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class)) {
      if ("X".equals(v.getText())) vars.add(v);
    }
    assertEquals(3 * repetitions + 1, vars.size());
    return vars;
  }
}
