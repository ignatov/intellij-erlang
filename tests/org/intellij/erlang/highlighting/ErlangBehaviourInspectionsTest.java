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
import org.intellij.erlang.inspection.ErlangNonExportedCallbackFunctionInspection;
import org.intellij.erlang.inspection.ErlangUndefinedCallbackFunctionInspection;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangBehaviourInspectionsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangUndefinedCallbackFunctionInspection.class,
                                ErlangNonExportedCallbackFunctionInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/highlighting/behaviour/";
  }

  public void testSimple() {
    myFixture.configureByFiles("b1.erl", "b2.erl", "test.erl");
    myFixture.checkHighlighting(true, false, false);
  }

  public void testCallbackImplementationsAreExported() {
    doCallbacksFixTest("Export all callbacks", "testImplemented.erl", "b1.erl", "b2.erl");
  }

  public void testCallbackImplementationsAreExportedOnce() {
    doCallbacksFixTest("Implement all callbacks", "testExported.erl", "b1.erl", "b2.erl");
  }

  public void testTest() {
    doCallbacksFixTest("Implement all callbacks", "test-qf.erl", "b1.erl", "b2.erl");
  }

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  private void doCallbacksFixTest(String launchIntention, String... files) {
    myFixture.configureByFiles(files);
    launchIntention(launchIntention);
    String expectedResultFile = FileUtil.getNameWithoutExtension(files[0]) + "-after.erl";
    myFixture.checkResultByFile(expectedResultFile);
  }
}
