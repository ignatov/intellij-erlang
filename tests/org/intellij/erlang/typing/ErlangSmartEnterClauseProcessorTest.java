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
import com.intellij.openapi.command.WriteCommandAction;
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
      "foo() -> ok;\n" +
      "foo() ->\n" +
        "foo() -> ok;");
  }

  public void testNotLastCaseClause() {
    doTest(
      "foo() ->\n" +
        "  case 1 of\n" +
        "    14 -> 30;<caret>\n" +
        "    123 -> 123\n" +
        "  end",
      "foo() ->\n" +
        "  case 1 of\n" +
        "    14 -> 30;\n" +
        "    _ ->\n" +
        "    123 -> 123\n" +
        "  end");
  }

  public void testCaseClause() {
    doTest(
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2;<caret>",
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2;\n" +
        "    _ -><caret>");
  }

  private void doTest(@NotNull String before, @NotNull String after) {
    myFixture.configureByText("a.erl", before);
    WriteCommandAction.writeCommandAction(getProject()).run(() -> {
      for (SmartEnterProcessor processor : SmartEnterProcessors.INSTANCE.forKey(ErlangLanguage.INSTANCE)) {
        processor.process(myFixture.getProject(), myFixture.getEditor(), myFixture.getFile());
      }
    });
    myFixture.checkResult(after);
  }
}
