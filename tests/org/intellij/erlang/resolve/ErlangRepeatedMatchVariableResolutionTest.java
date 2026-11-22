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

  // #1433: four more repetitions must not multiply the time of the resolve by 2^4
  public void testResolveAfterRepeatedMatchesDoesNotDouble() {
    timeOfResolve(12);
    long small = timeOfResolve(12);
    long big = timeOfResolve(16);
    assertTrue("12 repetitions: " + small + " ns, 16 repetitions: " + big + " ns", big < 8 * Math.max(small, 1_000_000L));
  }

  // #1481: the check of testResolveAfterRepeatedMatchesDoesNotDouble times one resolve of each size, so a busy machine
  // that pauses only the bigger resolve fails it although the resolve does not double; the check must hold on a busy runner
  public void testDoublingCheckHoldsOnBusyMachine() throws InterruptedException {
    List<Thread> load = new ArrayList<>();
    for (int i = 0; i < 3 * Runtime.getRuntime().availableProcessors(); i++) {
      Thread t = new Thread(() -> { while (!Thread.currentThread().isInterrupted()) Thread.onSpinWait(); });
      t.setDaemon(true);
      t.start();
      load.add(t);
    }
    try {
      for (int i = 0; i < 300; i++) {
        timeOfResolve(12);
        long small = timeOfResolve(12);
        long big = timeOfResolve(16);
        assertTrue("run " + i + ": 12 repetitions: " + small + " ns, 16 repetitions: " + big + " ns", big < 8 * Math.max(small, 1_000_000L));
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

  private long timeOfResolve(int repetitions) {
    List<ErlangQVar> vars = configure(repetitions);
    ErlangQVar last = vars.get(vars.size() - 1);
    long start = System.nanoTime();
    multiResolve(last);
    return System.nanoTime() - start;
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
