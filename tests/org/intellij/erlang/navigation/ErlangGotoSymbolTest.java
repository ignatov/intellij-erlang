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

package org.intellij.erlang.navigation;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.navigation.NavigationItem;
import com.intellij.psi.PsiElement;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangGotoSymbolTest extends BasePlatformTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myFixture.addFileToProject("module1.erl", "-module(module1).\n-record(rec, {}).\ntest() -> ok.\n");
    myFixture.addFileToProject("module2.erl", "-module(module2).\n-record(rec, {}).\ntest() -> ok.\n");
  }

  public void testName() {
    assertSameElements(gotoSymbol("test"), "module1.erl:test/0", "module2.erl:test/0");
  }

  public void testModuleQualifiedName() {
    assertSameElements(gotoSymbol("module1:test"), "module1.erl:test/0");
  }

  public void testOtherModuleQualifiedName() {
    assertSameElements(gotoSymbol("module2:test"), "module2.erl:test/0");
  }

  public void testModuleQualifiedNamePrefixes() {
    assertSameElements(gotoSymbol("mod1:te"), "module1.erl:test/0");
  }

  public void testModuleQualifiedRecord() {
    assertSameElements(gotoSymbol("module2:rec"), "module2.erl:rec");
  }

  public void testExactModuleQualifier() {
    addModulesNamedLikeModule1();
    assertSameElements(gotoSymbol("module1:test"), "module1.erl:test/0");
    assertSameElements(gotoSymbol("module1:te"), "module1.erl:test/0");
  }

  public void testExactModuleQualifierOfLongerName() {
    addModulesNamedLikeModule1();
    assertSameElements(gotoSymbol("my_module1:test"), "my_module1.erl:test/0");
  }

  public void testInexactModuleQualifier() {
    addModulesNamedLikeModule1();
    assertSameElements(gotoSymbol("odule1:test"), "module1.erl:test/0", "module10.erl:test/0", "my_module1.erl:test/0");
  }

  public void testUnknownModule() {
    assertEmpty(gotoSymbol("module3:test"));
  }

  private void addModulesNamedLikeModule1() {
    myFixture.addFileToProject("module10.erl", "-module(module10).\ntest() -> ok.\n");
    myFixture.addFileToProject("my_module1.erl", "-module(my_module1).\ntest() -> ok.\n");
  }

  private List<String> gotoSymbol(String pattern) {
    List<String> result = new ArrayList<>();
    for (Object item : myFixture.getGotoSymbolResults(pattern, false, null)) {
      PsiElement element = (PsiElement) ((StructureViewTreeElement) item).getValue();
      result.add(element.getContainingFile().getName() + ":" + ((NavigationItem) item).getPresentation().getPresentableText());
    }
    return result;
  }
}
