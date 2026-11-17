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

package org.intellij.erlang.typing;

import com.intellij.codeInsight.editorActions.smartEnter.SmartEnterProcessor;
import com.intellij.codeInsight.editorActions.smartEnter.SmartEnterProcessors;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupEx;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.util.containers.ContainerUtil;

import java.util.List;

import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangSmartEnterClauseProcessorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testFunctionClause() {
    doTest("foo(A, B, C) -> ok;<caret>",
      "foo(A, B, C) -> ok;\n" +
        "foo(A, B, C) ->");
  }

  public void testEmptyFunctionClause() {
    doTest("foo() -> ok;<caret>",
      "foo() -> ok;\n" +
        "foo() -><caret>");
  }
  
  public void testNotLastFunctionClause() {
    doTest(
      "foo() -> ok;<caret>\n" +
        "foo() -> ok;",
      """
        foo() -> ok;
        foo() ->
        foo() -> ok;""");
  }

  public void testNotLastCaseClause() {
    doTest(
      """
        foo() ->
          case 1 of
            14 -> 30;<caret>
            123 -> 123
          end""",
      """
        foo() ->
          case 1 of
            14 -> 30;
            _ ->
            123 -> 123
          end""");
  }

  public void testCaseClause() {
    doTest(
      """
        main(A) ->
          case A of
            1 -> 2;<caret>""",
      """
        main(A) ->
          case A of
            1 -> 2;
            _ -><caret>""");
  }

  public void testTerminatorProcessor() {
    doTest(
      "foo() -><caret>",
      "foo() ->,",
      ",", ";", "."
    );
  }

  public void testTerminatorProcessorWithExistingContent() {
    doTest(
      "foo() -> bar(<caret>)",
      "foo() -> bar(),",
      ",", ";", "."
    );
  }

  public void testTerminatorProcessorInCaseClause() {
    doTest(
      """
        case X of
          1 -> one<caret>
        end""",
      """
        case X of
          1 -> one
        end"""
    );
  }

  public void testTerminatorProcessorInFunction() {
    doTest(
      """
        foo() ->
          bar()<caret>
          baz().""",
      """
        foo() ->
          bar()<selection>,<caret></selection>
          baz().""",
      ",", ";", "."
    );
  }

  private void doTest(@NotNull String before, @NotNull String after, String... popupStrings) {
    if (popupStrings.length > 0) {
      TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
    }
    myFixture.configureByText("a.erl", before);
    WriteCommandAction.writeCommandAction(getProject()).run(() -> {
      for (SmartEnterProcessor processor : SmartEnterProcessors.INSTANCE.forKey(ErlangLanguage.INSTANCE)) {
        if (processor.process(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile())) break;
      }
    });

    if (popupStrings.length > 0) {
      LookupEx lookup = LookupManager.getActiveLookup(myFixture.getEditor());
      assertNotNull("Lookup should be shown", lookup);
      List<String> lookupStrings = ContainerUtil.map(lookup.getItems(), LookupElement::getLookupString);
      assertContainsElements(lookupStrings, popupStrings);
    }
    
    myFixture.checkResult(after);
  }
}
