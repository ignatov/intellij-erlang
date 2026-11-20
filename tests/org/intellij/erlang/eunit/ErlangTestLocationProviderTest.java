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

package org.intellij.erlang.eunit;

import com.intellij.execution.Location;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangTestLocationProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/eunit/location/";
  }

  public void testGeneratorTestLocation() {
    myFixture.configureByFile("g_tests.erl");

    PsiElement plain = getSingleLocation("g_tests:plain_test:-1");
    assertInstanceOf(plain, ErlangFunction.class);
    assertEquals("plain_test", ((ErlangFunction) plain).getName());

    // the hint that eunit_teamcity writes for the failing ?_assertEqual(1, 2) of gen_test_/0, on line 5
    PsiElement generated = getSingleLocation("g_tests:-gen_test_/0-fun-1-:5");
    ErlangFunction function = PsiTreeUtil.getParentOfType(generated, ErlangFunction.class, false);
    assertNotNull("the location of a generator test must lie inside its generator function, got " + generated, function);
    assertEquals("gen_test_", function.getName());
    assertEquals(4, myFixture.getEditor().getDocument().getLineNumber(generated.getTextOffset()));
  }

  private PsiElement getSingleLocation(String path) {
    List<Location> locations = ErlangTestLocationProvider.INSTANCE.getLocation(
      ErlangUnitRunConfigurationType.PROTOCOL, path, getProject(), GlobalSearchScope.projectScope(getProject()));
    assertEquals("locations of " + path, 1, locations.size());
    return locations.get(0).getPsiElement();
  }
}
