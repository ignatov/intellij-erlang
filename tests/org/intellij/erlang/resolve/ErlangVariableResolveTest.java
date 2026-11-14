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
import com.intellij.psi.PsiReference;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangVariableResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testComprehensionVariableShadowsParameter() {
    doTest("g3(X, L) -> [<caret>X || <target>X <- L], X.");
  }

  public void testCaseHeadBindingFromClause() {
    doTest("foo() -> case <target>X = bar() of _ -> <caret>X end.");
  }

  public void testCaseHeadBindingAfterEnd() {
    doTest("foo() -> case <target>X = bar() of _ -> ok end, <caret>X.");
  }

  public void testCaseHeadBindingRepeatedInPattern() {
    doTest("foo() -> case <target>X = bar() of X -> <caret>X end.");
  }

  public void testCaseHeadRematchOfParameter() {
    doTest("foo(<target>X) -> case X = bar() of _ -> <caret>X end.");
  }

  public void testCaseHeadRematchOfEarlierMatch() {
    doTest("foo() -> <target>X = 1, case X = bar() of _ -> <caret>X end.");
  }

  public void testCaseHeadRematchOfFunParameter() {
    doTest("foo() -> fun(<target>X) -> case X = bar() of _ -> <caret>X end end.");
  }

  private void doTest(String text) {
    int target = text.indexOf("<target>");
    int caretMarker = text.indexOf("<caret>");
    if (target > caretMarker) target -= "<caret>".length();
    myFixture.configureByText("a.erl", text.replace("<target>", ""));
    int caret = myFixture.getCaretOffset();
    PsiReference reference = myFixture.getFile().findReferenceAt(caret);
    assertNotNull(reference);
    PsiElement resolved = reference.resolve();
    assertNotNull(resolved);
    assertEquals(target, resolved.getTextRange().getStartOffset());
  }
}
