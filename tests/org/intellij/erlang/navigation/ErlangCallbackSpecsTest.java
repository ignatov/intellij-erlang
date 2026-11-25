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

package org.intellij.erlang.navigation;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangCallbackSpecsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  // before OTP 25 maybe is a plain atom: the title of maybe/1 is 'maybe'/1, the callback is written maybe(...)
  public void testCallbackNamedMaybeBeforeOtp25() {
    myFixture.addFileToProject("b.erl", """
      -module(b).
      -callback maybe(term()) -> ok.
      -callback foo(term()) -> ok.
      """);
    ErlangFile file = (ErlangFile) myFixture.configureByText("m.erl", """
      -module(m).
      -behaviour(b).
      -export([maybe/1, foo/1]).
      maybe(_) -> ok.
      foo(_) -> ok.
      """);
    ErlangFunction maybe = file.getFunction("maybe", 1);
    ErlangFunction foo = file.getFunction("foo", 1);
    assertNotNull(maybe);
    assertNotNull(foo);
    assertEquals(1, ErlangNavigationUtil.getCallbackSpecs(maybe).size());
    assertEquals(1, ErlangNavigationUtil.getCallbackSpecs(foo).size());
  }

  public void testCallbackNamedByQuotedReservedWord() {
    myFixture.addFileToProject("b.erl", """
      -module(b).
      -callback 'div'(term()) -> ok.
      """);
    ErlangFile file = (ErlangFile) myFixture.configureByText("m.erl", """
      -module(m).
      -behaviour(b).
      -export(['div'/1]).
      'div'(_) -> ok.
      """);
    ErlangFunction div = file.getFunction("div", 1);
    assertNotNull(div);
    assertEquals(1, ErlangNavigationUtil.getCallbackSpecs(div).size());
  }
}
