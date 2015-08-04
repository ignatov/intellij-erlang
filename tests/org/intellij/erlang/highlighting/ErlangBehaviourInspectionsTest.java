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

package org.intellij.erlang.highlighting;

import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.inspection.ErlangDuplicateBehaviourInspection;
import org.intellij.erlang.inspection.ErlangUndefinedCallbackFunctionInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedBehaviourInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangBehaviourInspectionsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangUndefinedCallbackFunctionInspection.class,
                                ErlangDuplicateBehaviourInspection.class,
                                ErlangUnresolvedBehaviourInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/behaviour/";
  }

  public void testHighlighting()                  { doHighlightingTest("testUndefined.erl", "b1.erl"); }
  public void testHighlightingSeveralBehaviours() { doHighlightingTest("testTwoUndefined.erl", "b1.erl", "b2.erl"); }
  public void testHighlightingDuplicate()         { doHighlightingTest("testDuplicate.erl", "b1.erl"); }
  public void testHighlightingUnresolved()        { doHighlightingTest("testUnresolved.erl", "b1.erl"); }

  public void testTwoConflicting()    { doHighlightingTest("testTwoConflicting.erl", "b1.erl", "b3.erl"); }
  public void testThreeConflicting()  { doHighlightingTest("testThreeConflicting.erl", "b1.erl", "b2.erl", "b3.erl"); }

  public void testCallbackImplementationsAreExported()     { doCallbacksFixTest("testImplemented.erl", "b1.erl"); }
  public void testCallbackImplementationsAreExportedOnce() { doCallbacksFixTest("testExported.erl", "b1.erl"); }
  public void testTest()                                   { doCallbacksFixTest("testBoth.erl", "b1.erl"); }

  public void testRemoveDuplicate() {
    myFixture.configureByFiles("testRemoveDuplicate.erl", "b1.erl", "b2.erl");
    launchIntention(ErlangDuplicateBehaviourInspection.FIX_MESSAGE);
    myFixture.checkResultByFile("testRemoveDuplicate-after.erl");
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  private void doHighlightingTest(String... files) {
    myFixture.configureByFiles(files);
    myFixture.checkHighlighting(true, false, false);
  }

  private void doCallbacksFixTest(String... files) {
    myFixture.configureByFiles(files);
    launchIntention(ErlangUndefinedCallbackFunctionInspection.FIX_MESSAGE);
    String expectedResultFile = FileUtil.getNameWithoutExtension(files[0]) + "-after.erl";
    myFixture.checkResultByFile(expectedResultFile);
  }
}
