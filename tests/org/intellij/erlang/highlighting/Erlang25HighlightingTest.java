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

package org.intellij.erlang.highlighting;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.inspection.ErlangIllegalGuardInspection;
import org.intellij.erlang.inspection.ErlangIllegalPatternInspection;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

public class Erlang25HighlightingTest extends ErlangHighlightingTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R25/", ErlangSdkRelease.V_25_0);
      }
    };
  }

  public void testErlang25SyntaxNoError() {
    enableErlang25SyntaxInspection();
    doTest();
  }

  // issue 1664
  public void testMaybeInCallArgument() {
    doTest();
  }

  public void testMaybeInCallArgumentNested() {
    doTest();
  }

  public void testControlEscapeAnyCharOtp25() {
    doTest();
  }

  public void testSigilControlEscapeBadNameOtp25() {
    doTest();
  }

  public void testIoFormatMapOrderModifiersBefore26() {
    myFixture.addFileToProject("io.erl", "-module(io).\n-export([format/2]).\nformat(_, _) -> ok.\n");
    doTest();
  }

  public void testUnresolvedMacrosFeature() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1596
  public void testUnresolvedMacrosFeatureQuoted() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1596
  public void testUnresolvedMacrosFeatureQuotedArity() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1596
  public void testUnresolvedMacrosFeatureQuotedOwnDefine() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureArity() {
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureArityLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureArity.erl");
  }

  public void testUnresolvedMacrosFeatureBare() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureOwnDefine() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureOwnDefineLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureOwnDefine.erl");
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineParens() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineBefore() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineArity() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineArityLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    myFixture.configureByText("a.erl", "-module(a).\n-export([f/0]).\n-define(FEATURE_ENABLED, true).\nf() -> ?FEATURE_ENABLED().\n");
    myFixture.checkHighlighting(true, false, false);
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineBare() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1565: -undef removes the predefined definition too
  public void testUnresolvedMacrosFeatureOwnDefineUndef() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineUndefInHeader() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    myFixture.addFileToProject("u.hrl", "-undef(FEATURE_ENABLED).\n-define(FEATURE_ENABLED, true).\n");
    myFixture.configureByText("a.erl", "-module(a).\n-export([f/0, true/0]).\n-include(\"u.hrl\").\nf() -> ?FEATURE_ENABLED().\ntrue() -> ok.\n");
    myFixture.checkHighlighting(true, false, false);
  }

  // issue 1565
  public void testUnresolvedMacrosFeatureOwnDefineUndefInIncludeLib() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    myFixture.addFileToProject("app/include/u.hrl", "-undef(FEATURE_ENABLED).\n-define(FEATURE_ENABLED, true).\n");
    myFixture.configureByText("a.erl", "-module(a).\n-export([f/0, true/0]).\n-include_lib(\"app/include/u.hrl\").\nf() -> ?FEATURE_ENABLED().\ntrue() -> ok.\n");
    myFixture.checkHighlighting(true, false, false);
  }

  // issue 1565: a module that includes the header may undefine the name
  public void testUnresolvedMacrosFeatureOwnDefineHeader() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    myFixture.addFileToProject("a.erl", "-module(a).\n-export([f/0, true/0]).\n-undef(FEATURE_ENABLED).\n-include(\"UnresolvedMacrosFeatureOwnDefineHeader.hrl\").\ntrue() -> ok.\n");
    myFixture.configureByFile("UnresolvedMacrosFeatureOwnDefineHeader.hrl");
    myFixture.checkHighlighting(true, false, false);
  }

  // issue 1565: in a header, a use that matches neither an own definition nor the predefined one
  public void testUnresolvedMacrosFeatureOwnDefineHeaderParams() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    myFixture.configureByFile("UnresolvedMacrosFeatureOwnDefineHeaderParams.hrl");
    myFixture.checkHighlighting(true, false, false);
  }

  // issue 1565: epp checks a -define body only where the macro is expanded
  public void testUnresolvedMacrosFeatureOwnDefineInDefineBody() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1564
  public void testUnresolvedMacrosFeatureOwnDefineOneArgument() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureOwnDefineOneArgumentLayerOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureOwnDefineOneArgument.erl");
  }

  public void testUnresolvedMacrosFeatureHeaderDefine() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureHeaderDefine.erl", "UnresolvedMacrosFeatureHeaderDefine.hrl");
  }

  public void testUnresolvedMacrosFeatureUndefOwnDefine() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  // issue 1597: erlc says "undefined macro 'FEATURE_ENABLED/1'"
  public void testUnresolvedMacrosFeatureUndefNoDefine() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineCases() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineConditional() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineHeader() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureUndefNoDefineHeader.erl", "UnresolvedMacrosFeatureUndefNoDefineHeader.hrl");
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineNestedHeader() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureUndefNoDefineNestedHeader.erl", "UnresolvedMacrosFeatureUndefNoDefineNestedHeader.hrl",
           "UnresolvedMacrosFeatureUndefNoDefineHeader.hrl");
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineHeaderConditional() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureUndefNoDefineHeaderConditional.erl", "UnresolvedMacrosFeatureUndefNoDefineHeaderConditional.hrl");
  }

  public void testUnresolvedMacrosFeatureUndefNoDefineHeaderConditionalInclude() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureUndefNoDefineHeaderConditionalInclude.erl",
           "UnresolvedMacrosFeatureUndefNoDefineHeaderConditionalInclude.hrl", "UnresolvedMacrosFeatureUndefNoDefineHeader.hrl");
  }

  // with the macro layer off, epp's checks of feature macros after an -undef are not made
  public void testUnresolvedMacrosFeatureUndefNoDefineOff() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureHeaderUndef() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureHeaderUndef.erl", "UnresolvedMacrosFeatureHeaderUndef.hrl");
  }

  public void testUnresolvedMacrosFeatureConditionalThenUndef() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureConditionalUndef() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest();
  }

  public void testUnresolvedMacrosFeatureNestedHeaderUndef() {
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    enableUnresolvedMacroInspection();
    doTest("UnresolvedMacrosFeatureNestedHeaderUndef.erl", "UnresolvedMacrosFeatureNestedHeaderUndef.hrl", "UnresolvedMacrosFeatureNestedHeaderUndef2.hrl");
  }

  // issue 1506
  public void testIllegalGuardCeilFloorMinMaxR25() {
    myFixture.configureByFile("IllegalGuardCeilFloorMinMaxR25.erl");
    myFixture.enableInspections(ErlangIllegalGuardInspection.class);
    myFixture.checkHighlighting(true, false, false);
  }

  public void testIllegalPatternSizeGuardBifsR25() {
    myFixture.configureByFile("IllegalPatternSizeGuardBifsR25.erl");
    myFixture.enableInspections(ErlangIllegalPatternInspection.class);
    myFixture.checkHighlighting(true, false, false);
  }
}
