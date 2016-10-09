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

package org.intellij.erlang.quickfixes;

public class ErlangGenerateSpecFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/generate_spec/";
  }

  private void doGenerateSpecTest() { doTest(ErlangGenerateSpecFix.NAME); }

  public void testNoArgs()          { doGenerateSpecTest(); }
  public void testWithArgs()        { doGenerateSpecTest(); }
  public void testMultipleClauses() { doGenerateSpecTest(); }

  public void testNoActionIfSpecExists() {
    String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    assertNoIntentionsAvailable(ErlangGenerateSpecFix.NAME, "Generate spec fix should not be available if spec already exists.");
  }
}
