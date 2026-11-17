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

package org.intellij.erlang.typing;

import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.command.WriteCommandAction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangLiveTemplateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
  }

  public void testCaseFollowedByExpression() {
    doTest(
      """
        foo(X) ->
          case<caret>
          ok.""",
      """
        foo(X) ->
          case X of
            1 -><caret>;
          end
          ok."""
    );
  }

  public void testCaseAtEndOfClause() {
    doTest(
      """
        foo(X) ->
          case<caret>""",
      """
        foo(X) ->
          case X of
            1 -><caret>;
          end"""
    );
  }

  public void testCaseWithGuard() {
    doTest(
      """
        foo(X) ->
          case<caret>
          ok.""",
      """
        foo(X) ->
          case X of
            Y when Y > 0 -><caret>;
          end
          ok.""",
      "Y when Y > 0"
    );
  }

  public void testCaseFinishedWithAllStopsEmpty() {
    doTabTest(
      """
        start_link() ->
          case<caret>
          gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).""",
      """
        start_link() ->
          case Expr of
            _ -><caret>;
          end
          gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).""",
      "", ""
    );
  }

  public void testCaseFinishedWithEmptyMatch() {
    doTabTest(
      """
        foo(X) ->
          case<caret>
          ok.""",
      """
        foo(X) ->
          case X of
            _ -><caret>;
          end
          ok.""",
      "X", ""
    );
  }

  public void testCaseFinishedWithTab() {
    doTabTest(
      """
        foo(X) ->
          case<caret>
          ok.""",
      """
        foo(X) ->
          case X of
            1 -><caret>;
          end
          ok.""",
      "X", "1"
    );
  }

  private void doTabTest(@NotNull String before, @NotNull String after, String @NotNull ... stops) {
    myFixture.configureByText("a.erl", before);
    myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB);
    TemplateState state = TemplateManagerImpl.getTemplateState(myFixture.getEditor());
    assertNotNull("Template should be started", state);
    for (String stop : stops) {
      if (!stop.isEmpty()) myFixture.type(stop);
      WriteCommandAction.runWriteCommandAction(getProject(), () -> state.nextTab());
    }
    assertTrue("Template should be finished", state.isFinished());
    myFixture.checkResult(after);
  }

  private void doTest(@NotNull String before, @NotNull String after) {
    doTest(before, after, "1");
  }

  private void doTest(@NotNull String before, @NotNull String after, @NotNull String match) {
    myFixture.configureByText("a.erl", before);
    myFixture.performEditorAction(IdeActions.ACTION_EXPAND_LIVE_TEMPLATE_BY_TAB);
    TemplateState state = TemplateManagerImpl.getTemplateState(myFixture.getEditor());
    assertNotNull("Template should be started", state);
    myFixture.type("X");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> state.nextTab());
    myFixture.type(match);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> state.gotoEnd(false));
    myFixture.checkResult(after);
  }
}
