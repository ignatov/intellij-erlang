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

public class ErlangNamedFunVariableResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String NAMED_FUN = """
    -module(named_fun).
    -export([foo/0]).

    foo() ->
      fun
        NamedFun
        (1, _) -> NamedFun(0, 0);
        NamedFun
        (0, NamedFun) ->
          NamedFun(1, 0);
        NamedFun(0, _) ->
          NamedFun(1, 1)
      end.
    """;

  public void testFirstClauseNameIsDeclaration()          { doTest(1, 0); }
  public void testCallInFirstClause()                     { doTest(2, 1); }
  public void testSecondClauseName()                      { doTest(3, 1); }
  public void testArgumentShadowingName()                 { doTest(4, 0); }
  public void testCallOfShadowingArgument()               { doTest(5, 4); }
  public void testThirdClauseName()                       { doTest(6, 1); }
  public void testCallInThirdClause()                     { doTest(7, 1); }

  /**
   * @param occurrence the 1-based index of the {@code NamedFun} variable in the text
   * @param expected   the 1-based index of the only resolve target, or 0 when the variable resolves to nothing
   */
  private void doTest(int occurrence, int expected) {
    myFixture.configureByText("named_fun.erl", NAMED_FUN);
    List<ErlangQVar> vars = new ArrayList<>();
    for (ErlangQVar var : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class)) {
      if ("NamedFun".equals(var.getText())) vars.add(var);
    }
    assertEquals(7, vars.size());

    PsiPolyVariantReference reference = (PsiPolyVariantReference) vars.get(occurrence - 1).getReference();
    List<Integer> targets = new ArrayList<>();
    for (ResolveResult result : reference.multiResolve(false)) {
      targets.add(vars.indexOf(result.getElement()) + 1);
    }
    assertEquals(expected == 0 ? List.of() : List.of(expected), targets);
    assertEquals(expected == 0 ? null : vars.get(expected - 1), reference.resolve());
  }
}
