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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.template.impl.TemplateManagerImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.intention.ErlangGenerateDocIntention;

public class ErlangGenerateDocIntentionTest extends ErlangQuickFixTestBase {
  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/generate_doc/";
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable());
  }

  private void doGenerateDocTest() {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    launchIntention(ErlangGenerateDocIntention.NAME);
    TemplateState state = TemplateManagerImpl.getTemplateState(myFixture.getEditor());
    assertNotNull("the intention should start a template", state);
    state.gotoEnd(false);
    myFixture.checkResultByFile(testName + "-after.erl");
  }

  private void doNoActionTest() {
    myFixture.configureByFile(getTestName(true) + ".erl");
    assertNoIntentionsAvailable(ErlangGenerateDocIntention.NAME);
  }

  private static ErlangGenerateDocIntention intention() {
    return new ErlangGenerateDocIntention() {};
  }

  public void testNoArgs()            { doGenerateDocTest(); }
  public void testWithArgs()          { doGenerateDocTest(); }
  public void testMultipleClauses()   { doGenerateDocTest(); }
  public void testAfterPlainComment() { doGenerateDocTest(); }
  public void testAfterDocumentedFunction() { doGenerateDocTest(); }

  public void testNoActionIfDocExists()          { doNoActionTest(); }
  public void testNoActionIfDocExistsBeforeSpec() { doNoActionTest(); }
  public void testNoActionOutsideFunction()      { doNoActionTest(); }
  public void testNoActionInFunctionBody()       { doNoActionTest(); }

  public void testNotAvailableInNonErlangFile() {
    myFixture.configureByText("a.txt", "foo<caret>() -> ok.");
    assertFalse(intention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }

  public void testInvokeInNonErlangFileThrows() {
    myFixture.configureByText("a.txt", "foo<caret>() -> ok.");
    try {
      intention().invoke(getProject(), myFixture.getEditor(), myFixture.getFile());
      fail("IncorrectOperationException expected");
    }
    catch (IncorrectOperationException e) {
      assertEquals("Only applicable to Erlang files.", e.getMessage());
    }
  }

  public void testInvokeOutsideFunctionThrows() {
    myFixture.configureByText("a.erl", "foo() -> o<caret>k.");
    try {
      intention().invoke(getProject(), myFixture.getEditor(), myFixture.getFile());
      fail("IncorrectOperationException expected");
    }
    catch (IncorrectOperationException e) {
      assertEquals("Cursor should be placed on Erlang function.", e.getMessage());
    }
    myFixture.checkResult("foo() -> o<caret>k.");
  }
}
