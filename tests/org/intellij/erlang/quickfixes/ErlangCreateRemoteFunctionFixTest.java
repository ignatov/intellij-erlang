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

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangUnresolvedFunctionInspection;
import org.intellij.erlang.psi.ErlangGlobalFunctionCallExpression;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangCreateRemoteFunctionFixTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @SuppressWarnings("unchecked")
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    myFixture.enableInspections(ErlangUnresolvedFunctionInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/create_remote_function/";
  }

  public void testRemoteCall()         { doTest("Create Function remoteCall_target:lll/3"); }
  public void testRemoteCallNoExport() { doTest("Create Function remoteCallNoExport_target:lll/0"); }
  public void testRemoteFun()          { doTest("Create Function remoteFun_target:lll/2"); }
  public void testArgumentNames()      { doTest("Create Function argumentNames_target:f/9"); }
  public void testQuotedName()         { doTest("Create Function quotedName_target:'my-func'/1"); }
  public void testExportOutsideIfdef() { doTest("Create Function exportOutsideIfdef_target:f/1"); }
  public void testExportOutsideIf()    { doTest("Create Function exportOutsideIf_target:f/1"); }
  public void testConditionalExportAll() { doTest("Create Function conditionalExportAll_target:f/1"); }
  public void testNewExportOutsideIfdef() { doTest("Create Function newExportOutsideIfdef_target:f/1"); }
  public void testReservedWordName()   { doTest("Create Function reservedWordName_target:'if'/1"); }
  public void testReservedWordFun()    { doTest("Create Function reservedWordFun_target:'end'/1"); }
  public void testFixAll()             { doTest("Fix all 'Unresolved function' problems in file"); }
  public void testSdkModule()          { doNoFixTest(true); }
  public void testUnresolvedModule()   { doNoFixTest(false); }

  public void testPreviewDoesNotChangeTargetModule() {
    myFixture.configureByFiles("remoteCall.erl", "remoteCall_target.erl");
    IntentionAction action = ContainerUtil.getFirstItem(
      myFixture.filterAvailableIntentions("Create Function remoteCall_target:lll/3"));
    assertNotNull(action);
    assertEquals("lll(N, Aaaa, OOO) ->\n  erlang:error(not_implemented).", myFixture.getIntentionPreviewText(action));
    myFixture.checkResultByFile("remoteCall_target.erl", "remoteCall_target.erl", true);
  }

  private void doTest(String quickFixName) {
    String testName = getTestName(true);
    myFixture.configureByFiles(testName + ".erl", testName + "_target.erl");
    String callerText = myFixture.getFile().getText();
    launchIntention(quickFixName);
    myFixture.checkResultByFile(testName + "_target.erl", testName + "_target-after.erl", true);
    assertEquals(callerText, myFixture.getFile().getText());
  }

  private void doNoFixTest(boolean moduleResolves) {
    myFixture.configureByFile(getTestName(true) + ".erl");
    ErlangGlobalFunctionCallExpression call =
      PsiTreeUtil.findChildOfType(myFixture.getFile(), ErlangGlobalFunctionCallExpression.class);
    assertNotNull(call);
    assertEquals(moduleResolves, ErlangPsiImplUtil.resolveToFile(call.getModuleRef()) != null);
    assertTrue(ContainerUtil.exists(myFixture.doHighlighting(),
                                    info -> "Unresolved function lll/1".equals(info.getDescription())));
    assertNoIntentionsAvailable("Create Function");
  }
}
