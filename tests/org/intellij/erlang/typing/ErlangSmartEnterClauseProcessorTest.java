/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangSmartEnterClauseProcessorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testFunctionClause() throws Exception {
    doTest("foo(A, B, C) -> ok;<caret>",
      "foo(A, B, C) -> ok;\n" +
        "foo(A, B, C) ->");
  }

  public void testEmptyFunctionClause() throws Exception {
    doTest("foo() -> ok;<caret>",
      "foo() -> ok;\n" +
        "foo() -><caret>");
  }
  
  public void testNotLastFunctionClause() throws Exception {
    doTest(
      "foo() -> ok;<caret>\n" +
        "foo() -> ok;",
      "foo() -> ok;\n" +
      "foo() ->\n" +
        "foo() -> ok;");
  }

  public void testNotLastCaseClause() throws Exception {
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

  public void testCaseClause() throws Exception {
    doTest(
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2;<caret>",
      "main(A) ->\n" +
        "  case A of\n" +
        "    1 -> 2;\n" +
        "    _ -><caret>");
  }

  private void doTest(String before, String after) {
    myFixture.configureByText("a.erl", before);
    final List<SmartEnterProcessor> processors = SmartEnterProcessors.INSTANCE.forKey(ErlangLanguage.INSTANCE);
    new WriteCommandAction(myFixture.getProject()) {
      @Override
      protected void run(Result result) throws Throwable {
        final Editor editor = myFixture.getEditor();
        for (SmartEnterProcessor processor : processors) {
          processor.process(myFixture.getProject(), editor, myFixture.getFile());
        }
      }
    }.execute();
    myFixture.checkResult(after);
  }
}
