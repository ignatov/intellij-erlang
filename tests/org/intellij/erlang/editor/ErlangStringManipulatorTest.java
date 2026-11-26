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

package org.intellij.erlang.editor;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.ElementManipulators;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangStringLiteral;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangStringManipulatorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0);
      }
    };
  }

  public void testSigilContentRange() {
    assertEquals("abc", ElementManipulators.getValueText(literalAtCaret("f() -> ~\"a<caret>bc\".")));
  }

  public void testTripleQuotedContentRange() {
    String value = ElementManipulators.getValueText(literalAtCaret("f() -> \"\"\"\n  {\"a\": <caret>1}\n  \"\"\"."));
    assertFalse(value, value.startsWith("\""));
    assertFalse(value, value.endsWith("\""));
  }

  public void testSigilContentChange() {
    ErlangStringLiteral literal = literalAtCaret("f() -> ~\"a<caret>bc\".");
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      ElementManipulators.handleContentChange(literal, "xyz");
    });
    assertEquals("f() -> ~\"xyz\".", myFixture.getEditor().getDocument().getText());
  }

  private ErlangStringLiteral literalAtCaret(String text) {
    myFixture.configureByText("a.erl", text);
    return getElementAtCaret(ErlangStringLiteral.class);
  }
}
