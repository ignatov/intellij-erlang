/*
 * Copyright 2012-2014 Sergey Ignatov
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
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.stubs.index.ErlangAllNameIndex;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Collection;

public class ErlangStubIndexTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testFunctionsAfterVerbatimSigilWithBackslash() {
    PsiFile file = myFixture.addFileToProject("verbatim.erl", """
      -module(verbatim).
      f() -> ~B"\\".
      g() -> ~S"\\".
      h() -> ~B[\\].
      i() -> "x".
      """);
    for (String name : new String[]{"f", "g", "h", "i"}) {
      Collection<ErlangNamedElement> elements = StubIndex.getElements(ErlangAllNameIndex.KEY, name, getProject(),
                                                                      GlobalSearchScope.allScope(getProject()),
                                                                      ErlangNamedElement.class);
      assertEquals(name, 1, elements.size());
      ErlangNamedElement element = elements.iterator().next();
      assertInstanceOf(element, ErlangFunction.class);
      assertEquals(file, element.getContainingFile());
    }
  }
}
