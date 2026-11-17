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

package org.intellij.erlang.stubs;

import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubElement;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.stubs.types.ErlangFileElementType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangFileStubTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // Up to version 7 an unterminated '-if(' swallowed the rest of the file, so stubs cached by
  // older versions lack the functions below it. The version must change to drop those stubs.
  public void testUnterminatedIfAttribute() {
    PsiFile file = myFixture.configureByText("p4.erl", "-module(p4).\n-if(\nf() -> ok.\ng() -> ok.\n");
    int functions = 0;
    for (StubElement<?> stub : ((PsiFileImpl) file).calcStubTree().getPlainList()) {
      if (stub.getPsi() instanceof ErlangFunction) functions++;
    }
    assertEquals(2, functions);
    assertTrue(ErlangFileElementType.INSTANCE.getStubVersion() > 7);
  }
}
