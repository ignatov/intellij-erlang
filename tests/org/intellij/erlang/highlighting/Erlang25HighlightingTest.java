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

  // issue 1506
  public void testIllegalGuardCeilFloorMinMaxR25() {
    myFixture.configureByFile("IllegalGuardCeilFloorMinMaxR25.erl");
    myFixture.enableInspections(ErlangIllegalGuardInspection.class);
    myFixture.checkHighlighting(true, false, false);
  }
}
